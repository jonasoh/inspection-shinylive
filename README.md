# Car inspection stats

Shiny app for exploring Finnish [car inspection data](https://trafi2.stat.fi/PXWeb/pxweb/en/TraFi/TraFi__Katsastuksen_vikatilastot/020_kats_tau_102.px/). 

This app was converted from [my previous implementation](https://github.com/jonasoh/car_inspection_stats) to now run using Shiny for Webassembly, making it much easier to deploy.

## Deploying

To deploy the app, run `preprocess_data.R` then upload the `site/` folder to your web server.
