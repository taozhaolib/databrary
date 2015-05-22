'use strict';

app.factory('storageService', [
  '$window', function ($window) {
    return {
      set: function (k, v, persist) {
        try {
          var store = persist && $window.localStorage || $window.sessionStorage;
          if (store)
            store.setItem(k, v);
        } catch (e) {
        }
      },
      get: function (k) {
        try {
          if ($window.sessionStorage)
            return $window.sessionStorage.getItem(k);
          if ($window.localStorage)
            return $window.localStorage.getItem(k);
        } catch (e) {
        }
      }
    };
  }]
);
