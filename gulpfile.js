/* jshint node: true */
"use strict";

var gulp = require("gulp");
var plumber = require("gulp-plumber");
var jshint = require("gulp-jshint");
var jscs = require("gulp-jscs");
var purescript = require("gulp-purescript");
var run = require("gulp-run");

var sources = [
  "src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
  "src/**/*.js",
  "bower_components/purescript-*/src/**/*.js"
];

gulp.task("lint", function() {
  return gulp.src("src/**/*.js")
    .pipe(jshint())
    .pipe(jshint.reporter())
    .pipe(jscs());
});

gulp.task("make", ["lint"], function() {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.pscMake({ ffi: foreigns }));
});

gulp.task("docs", function () {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.pscDocs({
      docgen: {
        "Data.Bifoldable": "docs/Data.Bifoldable.md",
        "Data.Bitraversable": "docs/Data.Bitraversable.md",
        "Data.Foldable": "docs/Data.Foldable.md",
        "Data.Traversable": "docs/Data.Traversable.md",
      }
    }));
});

gulp.task("dotpsci", function () {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.dotPsci());
});

gulp.task("test", ["make"], function() {
  return gulp.src(sources.concat(["test/Main.purs"]))
    .pipe(plumber())
    .pipe(purescript.psc({ main: "Test.Main"
                         , ffi: foreigns.concat(["test/Main.js"])
                         }))
    .pipe(run("node"));
});

gulp.task("default", ["make", "docs", "dotpsci"]);
