const fs = require("fs");
const R = require("ramda");

/*
 * Read the "tracks" file and compile it to paginated json data
 * The build assumes that tracks are separated by empty lines,
 * and that they're formatted like this
 * ------
 * artist
 * title
 * source
 * cover
 * ------
 */

const contents = fs.readFileSync("./tracks", "utf8");

const splitByEmptyLine = string => string.split("\n\n");

const splitByLine = string => string.split("\n");

const fields = ["artist", "title", "source", "cover"];

const customFormatToObject = lines =>
  lines.reduce((acc, line, i) => {
    if (fields[i] === "cover") {
      return R.assoc(fields[i], "/public/covers/" + line, acc);
    }
    return R.assoc(fields[i], line, acc);
  }, {});

const addStreamingInfo = track => {
  if (R.contains("youtube", track.source)) {
    return R.assoc(
      "youtube",
      { id: new URLSearchParams(new URL(track.source).search).get("v") },
      track
    );
  }
  if (R.contains("soundcloud", track.source)) {
    return R.pipe(
      R.assoc("source", track.source.split(" ")[0]),
      R.assoc("soundcloud", { stream_url: track.source.split(" ")[1] })
    )(track);
  }
  throw new Error("Source non reconnue");
};

const PAGE_SIZE = 2;
const paginate = tracks => {
  const pages = R.splitEvery(PAGE_SIZE, tracks);
  return pages.map((pageTracks, i) => ({
    tracks: pageTracks,
    next_href:
      pages.length === i + 1
        ? null
        : "/public/json/tracks/page_" + (i + 2) + ".json"
  }));
};

const writeJsonFile = (file, contents) =>
  fs.writeFileSync(file, JSON.stringify(contents));

const res = R.pipe(
  splitByEmptyLine,
  R.map(splitByLine),
  R.map(R.reject(R.equals(""))),
  R.map(customFormatToObject),
  R.map(addStreamingInfo),
  tracks => tracks.map((t, i) => R.assoc("id", i, t)),
  paginate,
  pages =>
    pages.map((page, i) =>
      writeJsonFile("public/json/tracks/page_" + (i + 1) + ".json", page)
    )
)(contents);
