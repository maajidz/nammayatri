const callbackMapper = require('presto-ui').callbackMapper;
exports.getKeyInSharedPrefKeysConfig = function (key) {
    return JBridge.getKeysInSharedPrefs(key);
  };

exports.getKeyInSharedPrefKeysConfigEff = function (key) {
    return JBridge.getKeysInSharedPrefs(key);
  };
