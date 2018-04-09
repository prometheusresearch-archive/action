const webpack = require('webpack');
const path = require('path');

const siteNodeModules = path.join(__dirname, 'node_modules');
const componentsPath = path.join(path.dirname(__dirname), 'components');
const componentsNodePath = path.join(
  path.dirname(__dirname),
  'components',
  'node_modules',
);

function isInternalPath(filename) {
  return filename.startsWith(componentsPath) && !filename.startsWith(componentsNodePath);
}

function isInternalReq(req) {
  const parts = req.split('!');
  const resource = parts[parts.length - 1];
  return resource.startsWith('components/');
}

module.exports = {
  webpack: (config, {dev, isServer, defaultLoaders}) => {
    config.externals = config.externals.map(external => {
      if (typeof external !== 'function') {
        return external;
      }
      return (ctx, req, cb) => {
        if (isInternalReq(req)) {
          return cb();
        } else {
          return external(ctx, req, cb);
        }
      };
    });
    config.resolve.modules = [siteNodeModules, 'node_modules'];
    config.module.rules.push({
      test: /\.+(js|jsx)$/,
      loader: defaultLoaders.babel,
      include(path) {
        if (isInternalPath(path)) {
          return true;
        } else {
          return false;
        }
      },
    });
    return config;
  },
};
