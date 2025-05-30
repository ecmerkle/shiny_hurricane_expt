# Shiny online experiment
This repo provides an example of how Shiny can be used for an online experiment, a concept that might be useful to other researchers. It is a simple study that involves making forecasts about hurricanes. In collaboration with the Good Judgment group at Penn, we used it to collect some Mechanical Turk data around 2015. The study didn't work out (the stimuli are very artificial), and nothing was published.

The hurricane animations in the stimuli folder are animated gifs created via ImageMagick. They follow the paths of real hurricanes, using data that came from NOAA. (The code and data for creating these animations seems to be lost.)

![hurricane animation](https://github.com/ecmerkle/shiny_hurricane_expt/blob/main/stimuli/hurricane8.gif)

The code was written around 2015, when Shiny was pretty new. At the time, I was tired of Qualtrics and looking for alternative ways to design an online study. The study relies on a bunch of conditional statements to determine what should be displayed when. It also writes some results to file to keep track of running scores, then reads the file at the end to compute total scores.

When the study first went live on Mechanical Turk, we crashed the server because too many people were doing the study at once. But we eventually worked that out, and I ended up favoring this approach over Qualtrics.
