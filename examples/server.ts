// Using this for now for testing because it's easy.
// In the future I'll replace this example with some webserver platform.
//
// Run with:
// $ deno run --allow-net --allow-run examples/server.ts posts
//
// Start listening on port 8080 of localhost.

const program = Deno.args[0];

if (program !== "posts" && program !== "pg") {
    console.log("Must pass example program as first argument. Options: posts, pg.")
    Deno.exit(1);
}

const server = Deno.listen({ port: 8080 });
console.log(`HTTP webserver running.  Access it at:  http://localhost:8080/`);

for await (const conn of server) {
  serveHttp(conn);
}

async function serveHttp(conn: Deno.Conn) {
  const httpConn = Deno.serveHttp(conn);
  for await (const requestEvent of httpConn) {
    if (requestEvent.request.method === "POST") {
      const body = await requestEvent.request.json();

      const proc = new Deno.Command(`examples/${program}`, {
        args: [body.query.trim()],
      });
      const output = await proc.output();

      if (output.code == 0) {
        const result = new TextDecoder().decode(output.stdout);

        requestEvent.respondWith(
          new Response(result, {
            status: 200,
            headers: {
              "Content-Type": "application/json",
            },
          }),
        );
      } else {
        const result = new TextDecoder().decode(output.stderr);
                console.log(result);
        requestEvent.respondWith(new Response(result, { status: 500 }));
      }
    } else {
      requestEvent.respondWith(new Response("NotFound", { status: 404 }));
    }
  }
}
