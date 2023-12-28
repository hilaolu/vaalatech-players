import { $$fetch } from "./Main.bs.js";

export default {
    async fetch(request) {
        return await $$fetch(request);
    }
}

