var fs = get('fs');
var electron = get('electron')

var ipcRenderer = electron.ipcRenderer;

function projectDir(path) {
  return './project/' + path;
}

function partsDir(path) {
  if (typeof path === 'undefined') {
    return projectDir('parts');
  }
  return projectDir('parts/' + path);
}

function readPackage() {
  return fs.readFileSync(
    projectDir('package.json'),
    'utf-8'
  )
}

function readParts() {
  return fs.readdirSync(partsDir())
    .map(function (partName) {
      return {
        name: partName,
        data:
          fs.readFileSync(
            partsDir(partName),
            'utf-8'
          )
      };
    })
}


ipcRenderer.on('init', function (event, payload) {

  var flags = {
    package: readPackage(),
    parts: readParts(),
    enginePortNumber: payload.port
  };

  console.log(JSON.stringify(flags));

  var app = Elm.Main.init({
    flags: flags
  });

  function toElm(type, payload) {
    app.ports.fromJs.send({
      type: type,
      payload: payload
    });
  }

  var actions = {
    savePartToDisk: function (payload) {
      fs.writeFileSync(
        partsDir(payload.name),
        payload.data
      );
    },
    savePackageToDisk: function (payload) {
      fs.writeFileSync(
        projectDir('package.json'),
        payload
      );
    }
  }

  function jsMsgHandler(msg) {
    var action = actions[msg.type];
    if (typeof action === "undefined") {
      console.log("Unrecognized js msg type ->", msg.type);
      return;
    }
    action(msg.payload);
  }

  app.ports.toJs.subscribe(jsMsgHandler);

});




