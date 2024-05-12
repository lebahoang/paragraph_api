from typing import Any
import httpx
import asyncio
async def _call(request_str: str, client: httpx.AsyncClient) -> dict[str, Any]:
    resp = await client.get(request_str)
    data = resp.json()
    if type(data) is list:
        data = data[0]
    else:
        data = {}
    return data

async def call(request_strs: list[str]) -> list[dict[str, Any]]:
    async with httpx.AsyncClient() as client:
        tasks = []
        for request_str in request_strs:
            tasks.append(_call(request_str, client))
        return await asyncio.gather(*tasks)
        # rs = []
        # for request_str in request_strs:
        #     resp = await client.get(request_str)
        #     data = resp.json()
        #     if type(data) is list:
        #         data = data[0]
        #     else:
        #         data = {}
        #     rs.append(data)
        # return rs