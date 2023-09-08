# FulmaExtensions
## Build

### Run examples
```bash
build -t Pack
cd examples
npm install
dotnet paket update FulmaExtensions
npm run deploy && npm run vite:rundeploy
# or run command below to developing
npm run dev
```
