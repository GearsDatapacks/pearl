import { Ok, Error } from "./gleam.mjs";

export function strip_prefix(string, prefix) {
  if (string.startsWith(prefix)) {
    return new Ok(string.slice(prefix.length));
  } else {
    return new Error(undefined);
  }
}
