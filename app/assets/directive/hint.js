'use strict';

module.directive('hint', [
  'pageService', function (page) {
    var hints = {};

    angular.forEach(page.constants.permission, function (a) {
      hints['permission-' + a] =
        page.constants.message('access.' + a, 'You');
      if ('access.edit.' + a + '.who' in page.constants.messages)
        hints['access-edit-' + a] =
          page.constants.message('access.edit.' + a + '.who', 'You');
    });

    angular.forEach(page.constants.consent, function (a) {
      hints['consent-' + a] =
        page.constants.message('consent.' + a);
    });

    angular.forEach(page.constants.classification, function (a) {
      hints['classification-' + a] =
        page.constants.message('classification.' + a);
    });

    angular.forEach(page.constants.format, function (a) {
      hints['format-' + a.extension] =
        a.name;
    });

    angular.forEach(['dataset', 'study', 'record', 'slot', 'asset'], function (a) {
      hints['object-' + a] =
        page.constants.message('object.tip.' + a);
    });

    angular.forEach(['up', 'null', 'down'], function (a) {
      hints['tags-vote-' + a] =
        page.constants.message('tags.vote.' + a);
    });

    angular.forEach(hints, function (hint, name) {
      new page.tooltips({
        live: true,
        $target: '.hint-' + name,
        message: hint
      });
    });

    var link = function ($scope, $element, $attrs) {
      $element.addClass('hint-' + $attrs.hint);
    };

    return {
      restrict: 'A',
      link: link,
    };
  }
]);
