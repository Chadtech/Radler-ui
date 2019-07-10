var fs = get('fs');
var electron = get('electron');

var ipcRenderer = electron.ipcRenderer;

function projectDir(path) {
  return './project/' + path;
}

function partExtension(fileName) {
  return fileName + ".part";
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

function readTrackers() {
  try {
    return JSON.parse(
      fs.readFileSync(
        projectDir('trackers.json'),
        'utf-8'
      )
    )
  } catch (err) {
    return [];
  }

}

function readParts() {
  return fs.readdirSync(partsDir())
    .filter(function (fileName) {
      return fileName.slice(-5) === ".part";
    })
    .map(function (partName) {
      return {
        name: partName.slice(0, -5),
        data:
          fs.readFileSync(
            partsDir(partName),
            'utf-8'
          )
      };
    })
}

ipcRenderer.on('init', function (event, payload) {
  console.log(readTrackers())
  var flags = {
    package: readPackage(),
    parts: readParts(),
    trackers: readTrackers(),
    enginePortNumber: payload.port
  };

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
        partsDir(partExtension(payload.name)),
        payload.data
      );
    },
    savePackageToDisk: function (payload) {
      fs.writeFileSync(
        projectDir('package.json'),
        payload
      );
    },
    deletePartFromDisk: function (payload) {
      fs.unlink(partsDir(partExtension(payload.name)), function (err) {
        if (err === null) {
          toElm("partDeleted", payload.index);
        } else {
          toElm("partDeleted", String(err));
        }
      });
    },
    saveTrackersToDisk: function(payload){
      fs.writeFileSync(
        projectDir('trackers.json'),
        JSON.stringify(payload)
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




