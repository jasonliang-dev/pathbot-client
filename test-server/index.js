const express = require("express");
const cors = require("cors");

const app = express();
const port = 3333;

const rootMazeNode = {
  status: "in-progress",
  exits: ["N", "E", "S"],
  locationPath: "/pathbot/rooms/0"
};

app.use(cors());
app.use(express.json());

app.post("/pathbot/start", (req, res) => res.json(rootMazeNode));

app.post("/pathbot/rooms/:room", (req, res) => {
  if (req.params.room === "0" && req.body.direction === "E") {
    res.json({ status: "finished" });
    return;
  }

  const nextRoom = delta => ({
    status: "in-progress",
    exits: ["N", "S"],
    locationPath: `/pathbot/rooms/${+req.params.room + delta}`
  });

  switch (req.body.direction) {
    case "N":
      if (req.params.room === "1") {
        res.json(rootMazeNode);
      } else {
        res.json(nextRoom(-1));
      }
      break;
    case "S":
      if (req.params.room === "-1") {
        res.json(rootMazeNode);
      } else {
        res.json(nextRoom(1));
      }
      break;
    default:
      res.status(400).json({ message: "bad move" });
  }
});

app.listen(port, () =>
  console.log(`Pathbot testing server listening on port ${port}!`)
);
