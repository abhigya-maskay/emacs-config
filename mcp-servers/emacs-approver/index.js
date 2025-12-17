#!/usr/bin/env node
/**
 * MCP Permission Server for Emacs
 *
 * This MCP server delegates tool approval decisions to Emacs via a TCP socket.
 * When Claude needs to use a tool, it calls this server's "approve" tool,
 * which forwards the request to Emacs for user approval.
 */

import { Server } from "@modelcontextprotocol/sdk/server/index.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
} from "@modelcontextprotocol/sdk/types.js";
import net from "net";

const EMACS_PORT = parseInt(process.env.EMACS_APPROVAL_PORT || "9876", 10);
const EMACS_HOST = process.env.EMACS_APPROVAL_HOST || "127.0.0.1";

const server = new Server(
  { name: "emacs-approver", version: "1.0.0" },
  { capabilities: { tools: {} } }
);

// List available tools
server.setRequestHandler(ListToolsRequestSchema, async () => ({
  tools: [
    {
      name: "approve",
      description: "Request approval from Emacs for a Claude Code tool call",
      inputSchema: {
        type: "object",
        properties: {
          tool_name: {
            type: "string",
            description: "Name of the tool requesting approval (e.g., Bash, Write, Edit)"
          },
          input: {
            type: "object",
            description: "The input parameters for the tool"
          }
        },
        required: ["tool_name", "input"]
      }
    }
  ]
}));

// Handle tool calls
server.setRequestHandler(CallToolRequestSchema, async (request) => {
  if (request.params.name !== "approve") {
    throw new Error(`Unknown tool: ${request.params.name}`);
  }

  const { tool_name, input } = request.params.arguments;

  try {
    // Connect to Emacs socket server and get approval
    const response = await requestApprovalFromEmacs(tool_name, input);
    return {
      content: [{ type: "text", text: response }]
    };
  } catch (error) {
    // If we can't connect to Emacs, deny by default
    console.error(`Failed to connect to Emacs: ${error.message}`);
    return {
      content: [{
        type: "text",
        text: JSON.stringify({
          behavior: "deny",
          message: `Could not connect to Emacs approval server: ${error.message}`
        })
      }]
    };
  }
});

/**
 * Connect to Emacs socket server and request approval
 */
function requestApprovalFromEmacs(toolName, input) {
  return new Promise((resolve, reject) => {
    const client = net.createConnection({ port: EMACS_PORT, host: EMACS_HOST }, () => {
      // Send the approval request
      const request = JSON.stringify({ tool: toolName, input: input });
      client.write(request + "\n");
    });

    let data = "";

    client.on("data", (chunk) => {
      data += chunk.toString();
      // Check if we have a complete response (ends with newline)
      if (data.includes("\n")) {
        client.end();
      }
    });

    client.on("end", () => {
      resolve(data.trim());
    });

    client.on("error", (err) => {
      reject(err);
    });

    // Timeout after 1 hour (user might step away)
    client.setTimeout(3600000, () => {
      client.destroy(new Error("Connection timeout"));
    });
  });
}

// Run the server with stdio transport
async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error("Emacs Approver MCP server running on stdio");
}

main().catch((error) => {
  console.error("Server error:", error);
  process.exit(1);
});
