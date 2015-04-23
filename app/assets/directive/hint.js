'use strict';

app.directive('hint', [
  'pageService', function (page) {
    var hints = {};

    _.each(page.constants.permission, function (a) {
      hints['permission-' + a] =
        page.constants.message('access.' + a, 'You');
      if ('access.edit.' + a + '.who' in page.constants.messages)
        hints['access-edit-' + a] =
          page.constants.message('access.edit.' + a + '.who', 'You');
    });

    _.each(page.constants.release, function (a) {
      hints['consent-' + a] =
        page.constants.message('consent.' + a);
    });

    _.each(page.constants.release, function (a) {
      hints['classification-' + a] =
        page.constants.message('classification.' + a);
      hints['excerpt-' + a] =
        page.constants.message('classification.excerpt.' + a);
    });

    _.each(page.constants.format, function (a) {
      hints['format-' + a.extension] =
        a.name;
    });

    _.each(['slot'], function (a) {
      hints['action-' + a] =
        page.constants.message('hint.action.' + a);
    });

    _.each(['up', 'null'], function (a) {
      hints['tags-vote-' + a] =
        page.constants.message('tags.vote.' + a);
    });

    _.each(hints, function (hint, name) {
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
