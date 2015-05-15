
To start:
GHC_PACKAGE_PATH=$(cabal sandbox hc-pkg list | grep '^/.*\.cabal-sandbox') repli 3000 8080

To stop (GHC takes over signals):
ps ax |grep  repli | cut -d\  -f1 | xargs kill -9
