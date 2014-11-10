'use strict';

app.directive('volumeEditOverviewForm', [
  'pageService', '$routeParams',
  function (page, $routeParams) {
    var link = function ($scope) {
      var volume = $scope.volume;
      var form = $scope.volumeEditOverviewForm;

      function init(volume) {
        var citation = volume.citation || {};
        form.data = {
          name: volume.name,
          alias: volume.alias,
          body: volume.body,
          citation: {
            head: citation.head,
            title: citation.title,
            url: citation.url,
            authors: citation.authors ? citation.authors.slice(0) : [],
          }
        };
        form.data.citation.authors.push('');
      }
      init(volume || {});

      form.setAutomatic = function (auto) {
        form.automatic = auto;
        form.validator.client({
          'citation.url': {
            tips: page.constants.message('volume.edit.citation.' + (form.automatic ? 'doi' : 'url') + '.help')
          }
        }, true);
      };

      form.save = function () {
        form.data.citation.authors = form.data.citation.authors
          .filter(function (author) {
            return author;
          });

        (volume ?
          volume.save(form.data) :
          page.models.Volume.create(form.data, $routeParams.owner))
          .then(function (vol) {
            form.validator.server({});

            form.messages.add({
              type: 'green',
              countdown: 3000,
              body: page.constants.message('volume.edit.success'),
            });

            init(vol);
            form.$setPristine();

            if (!volume)
              page.$location.url(vol.editRoute());
          }, function (res) {
            form.validator.server(res);
          });
      };

      form.autoDOI = function () {
        var doi = page.constants.regex.doi.exec(form.data.citation.url);

        if (!doi || !doi[1]) {
          return;
        }

        page.models.cite(doi[1])
          .then(function (res) {
            form.data.name = res.title;
            form.data.citation = res;
            if (form.data.citation.authors)
              form.data.citation.authors.push('');
            else
              form.data.citation.authors = [''];
            delete res.title;

            form.setAutomatic(false);

            form.messages.add({
              type: 'green',
              countdown: 3000,
              body: page.constants.message('volume.edit.autodoi.citation.success'),
            });
          }, function () {
            form.messages.add({
              type: 'red',
              countdown: 5000,
              body: page.constants.message('volume.edit.autodoi.citation.error'),
            });
          });
      };

      form.authorChange = function () {
        if (form.data.citation.authors[form.data.citation.authors.length-1] !== '')
          form.data.citation.authors.push('');
      };

      form.authorRemove = function (i) {
        form.data.citation.authors[i] = i === form.data.citation.authors.length-1 ? '' : null;
        form.$setDirty();
      };

      var validate = {};
      ['name', 'body', 'alias', 'citation.head', 'citation.url', 'citation.year'].forEach(function (f) {
        validate[f] = {
          tips: page.constants.message('volume.edit.' + f + '.help')
        };
      });
      form.validator.client(validate, true);
      form.setAutomatic(!volume);

      form.scrollFn = page.display.makeFloatScrollFn($('.veo-float'), $('.veo-float-floater'), 24*1.5);
      page.$w.scroll(form.scrollFn);
    };

    return {
      restrict: 'E',
      templateUrl: 'volume/editOverview.html',
      link: link
    };
  }
]);
