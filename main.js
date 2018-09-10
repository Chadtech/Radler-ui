var electron = require("electron")
var cp = require("child_process");

var app = electron.app;
var BrowserWindow = electron.BrowserWindow;
var main = {
  window: null,
  engine: null
};

function toUi(msg) {
  if (main.window !== null) {
    main.window.webContents.send("from-engine", msg);
  }
}

function decodeUiMsg(pieces) {
  switch (pieces[0]) {
    case "ready":
      return {
        type: pieces[0],
        payload: null
      };
  }
}

function toEngine(msg) {
  if (main.engine === null) {
    return;
  }

  switch (msg.type) {
    case "play":
      main.engine.stdin.write([
        "play;",
        msg.payload.from,
        msg.payload.to
      ].join(" "));
      break;
  }
  main.engine.stdin.end();
}

function createWindow() {
  main.window = new BrowserWindow()
  main.window.setSimpleFullScreen(true);

  main.window.loadFile("public/index.html")
  main.window.openDevTools()
  main.window.webContents.on("did-finish-load", function () {
    main.engine = cp.spawn("./dist/build/radler/radler", {
      stdio: [
        "pipe",
        "pipe",
        process.stderr
      ]
    });
    main.engine.stdin.setEncoding("utf-8");
    main.engine.stdout.on("data", function (data) {
      console.log("DATA", String(data));
      toUi(decodeUiMsg(String(data).split(";")));
    });
    main.engine.on("close", function (code) {
      console.log("child process exited with code " + code);
    });
  });

  main.window.on("closed", function () {
    main.window = null
    if (main.engine !== null) {
      main.engine.kill("SIGINT");
    }
  })
}

app.on("ready", createWindow)

app.on("window-all-closed", function () {
  if (process.platform !== "darwin") {
    app.quit()
  }
})

app.on("activate", function () {
  if (main.window === null) {
    createWindow()
  }
})