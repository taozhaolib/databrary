'use strict';

module.directive('form', [
  'pageService', function (page) {
    var link = function ($scope, $element, $attrs) {
      if (!$attrs.name) {
        return;
      }

      var form = $scope[$attrs.name];
      form.messages = page.messages;

      var unclaimed = {};

      form.$element = $element;

      if (angular.isDefined($attrs.novalidate)) {
        form.validators = {};
        form.validator = {
          server: function (res, replace) {
            if ($.isEmptyObject(res)) {
              res.data = {};
            } else if (!angular.isObject(res.data)) {
              form.messages.addError({
                body: page.constants.message('error.generic'),
                report: res,
              });
              return;
            }

            var name;
            for (name in form.validators) {
              if (form.validators.hasOwnProperty(name)) {
                form.validators[name].server(res.data[name] || {}, replace);
              } else if (form.messages) {
                form.messages.add({
                  type: 'red',
                  closeable: true,
                  body: Array.isArray(res.data[name]) ? res.data[name].join(', ') : res.data[name],
                });
              }
            }

            for (name in res.data) {
              if (res.data.hasOwnProperty(name) && form.validators[name]) {
                form.validators[name].server(res.data[name], replace);
              } else if (form.messages) {
                form.messages.add({
                  type: 'red',
                  closeable: true,
                  body: Array.isArray(res.data[name]) ? res.data[name].join(', ') : res.data[name],
                });
              }
            }
          },

          clearServer: function () {
            angular.forEach(form.validators, function (validator) {
              validator.server({}, true);
            });
          },

          client: function (data, replace) {
            for (var name in data) {
              if (!data.hasOwnProperty(name)) {
                continue;
              } else if (form.validators[name]) {
                form.validators[name].client(data[name], replace);
              } else {
                unclaimed[name] = data[name];
              }
            }
          },

          add: function (name, validator) {
            form.validators[name] = validator;

            if (unclaimed[name]) {
              validator.client(unclaimed[name], true);
              delete unclaimed[name];
            }
          },
        };
      }
    };

    return {
      restrict: 'E',
      link: link,
    };
  }
]);
