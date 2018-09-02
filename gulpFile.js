var gulp = require("gulp");
var source = require("vinyl-source-stream");
var buffer = require("vinyl-buffer");
var cp = require("child_process");
var browserify = require("browserify");
var util = require("gulp-util");


var paths = {
  public: "./public",
  dist: "./dist",
  mainElm: "./ui-src/Main.elm",
  elm: "./ui-src/**/*.elm",
  mainJs: "./ui-src/app.js",
  js: "./ui-src/*.js",
  electron: "./main.js",
};

gulp.task("js", function () {
  return browserify(paths.mainJs)
    .bundle()
    .pipe(source("app.js"))
    .pipe(buffer())
    .pipe(gulp.dest(paths.public));
});


gulp.task("elm", function () {
  util.log(util.colors.cyan("Elm"), "starting");
  cp.spawn("elm", [
    "make",
    paths.mainElm,
    "--output",
    paths.public + "/elm.js",
  ], {
      stdio: 'inherit'
    }).on("close", function (code) {
      util.log(util.colors.cyan("Elm"), "closed");
    });
});

gulp.task("electron", function () {
  cp.spawn("electron", [
    paths.electron
  ], {
      stdio: 'inherit'
    }).on("close", function (code) {
      util.log(util.colors.cyan("Electron"), "closed");
    });
});

gulp.watch(paths.elm, ["elm"]);
gulp.watch(paths.js, ["js"]);

gulp.task("default", ["elm", "js"]);
