# ApiMap

## AdminApiService
	mkYesod "AdminApiService" [parseRoutes|
	/setresponse SetResponseR POST
	/responsecnt ResponseCntR GET
	/responses ResponsesR GET
	|]

### /setresponse example
	methodPath=qwe/xcv/fgh
	responseBody="{\"custom\": \"response\",\n\"body\": \"text_value\"\n}"
	responseHeaders="[(\"X-Header-Name1\", \"val1\"), (\"X-Header-Name2\", \"val2\")]"

## ClientService
	mkYesod "ClientService" [parseRoutes|
	/*MethodPieces ResponseR
	|]
