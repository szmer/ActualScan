set -e

# Run scrapyd in the backgroud and re-foreground it after deployment.
scrapyd &
cd genscrap
scrapyd-deploy
cd ..
