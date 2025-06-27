# Run_Shinylive_app

library(shinylive)

# Create 'docs' and 'app' folders. The app folder contains app.R and df_clean.RDS


## Run the following in an R session to serve the app:

shinylive::export(appdir = ".", destdir = "docs")

## Run the app locally:
httpuv::runStaticServer("docs")

fs::dir_ls(".")

# Tips for a Successful Export
# Check Asset Versions:
shinylive::assets_info() # to verify that the correct web assets are installed and cached.

### Update Packages if Needed:
shinylive::assets_download() # to update your assets.

### Clean Up Old Assets:
shinylive::assets_cleanup() # to remove outdated asset versions from your cache.
# Or to remove a specific version:
 
# shinylive::assets_remove("0.3.0")



Tidyverse is up to date for my machine.
Here are the errors: 
shinylive.js:35463 
preload error:Error: package or namespace load failed for ‘tidyverse’:
 shinylive.js:35463 
preload error: .onAttach failed in attachNamespace() for 'tidyverse', details:
 shinylive.js:35463 
preload error:  call: NULL
shinylive.js:35463 
preload error:  error: package or namespace load failed for ‘ggplot2’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 shinylive.js:35463 
preload error: there is no package called ‘munsell’
shinylive.js:35463 
preload error:Error: object 'app_blphko0cb4vtyx7uix87' not found
shinylive.js:32284 
Uncaught (in promise) Error: Unexpected response type: "null", expected "list".
at handleHttpuvRequests (shinylive.js:32284:17)
at async makeHttpuvRequest (shinylive.js:32240:3)
at async WebRWorkerProxy.makeRequest (shinylive.js:33939:5)