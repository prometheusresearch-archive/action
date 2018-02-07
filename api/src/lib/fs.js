/**
 * @flow
 */

const crypto = require('crypto');
const fs = require('fs');
const path = require('path');
const os = require('os');

import {promisify} from './Promise';

// Promote some of the fs built-ins
export const stat: (path: string) => Promise<fs.Stats> = promisify(fs.stat);
export const lstat: (path: string) => Promise<fs.Stats> = promisify(fs.lstat);
export const readdir: (path: string, opts: void) => Promise<Array<string>> = promisify(
  fs.readdir,
);
export const rename: (oldPath: string, newPath: string) => Promise<void> = promisify(
  fs.rename,
);
export const exists: (path: string) => Promise<boolean> = promisify(fs.exists, true);
export const existsSync = fs.existsSync;
export const realpath: (p: string) => Promise<string> = promisify(fs.realpath);
export const readlink: (p: string) => Promise<string> = promisify(fs.readlink);
export const realpathSync: (p: string) => string = fs.realpathSync;
export const unlink: (prefix: string) => Promise<string> = promisify(fs.unlink);
export const readFileBuffer: (p: string) => Promise<Buffer> = promisify(fs.readFile);
export const writeFile: (
  path: string,
  data: Buffer | string,
  encdogin?: string,
) => Promise<void> = promisify(fs.writeFile);
export const chmod: (path: string, mode: number | string) => Promise<void> = promisify(
  fs.chmod,
);
export const writeFileSync = fs.writeFileSync;

// mkdtemp
export const _mkdtemp: string => Promise<string> = promisify(fs.mkdtemp);
export const _mkdtempSync = fs.mkdtempSync;

export function mkdtemp(prefix: string) {
  const root = os.tmpdir();
  return _mkdtemp(path.join(root, prefix));
}

export async function readFile(p: string): Promise<string> {
  const data = await readFileBuffer(p);
  return data.toString('utf8');
}

export function readFileSync(p: string) {
  return fs.readFileSync(p, 'utf8');
}

export async function readJson(p: string, parse: string => mixed = JSON.parse) {
  const data = await readFile(p);
  return parse(data);
}

export function readJsonSync(p: string, parse: string => mixed = JSON.parse) {
  const data = readFileSync(p);
  return parse(data);
}

export const fsSymlink: (
  target: string,
  path: string,
  type?: 'dir' | 'file' | 'junction',
) => Promise<void> = promisify(fs.symlink);

export async function symlink(src: string, dest: string): Promise<void> {
  try {
    const stats = await lstat(dest);

    if (stats.isSymbolicLink() && (await exists(dest))) {
      const resolved = await realpath(dest);
      if (resolved === src) {
        return;
      }
    }

    await unlink(dest);
  } catch (err) {
    if (err.code !== 'ENOENT') {
      throw err;
    }
  }

  try {
    if (process.platform === 'win32') {
      // use directory junctions if possible on win32, this requires absolute paths
      await fsSymlink(src, dest, 'junction');
    } else {
      // use relative paths otherwise which will be retained if the directory is moved
      const relative = path.relative(
        fs.realpathSync(path.dirname(dest)),
        fs.realpathSync(src),
      );
      await fsSymlink(relative, dest);
    }
  } catch (err) {
    if (err.code === 'EEXIST') {
      // race condition
      await symlink(src, dest);
    } else {
      throw err;
    }
  }
}
