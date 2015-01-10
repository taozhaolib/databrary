'use strict';

app.directive('volumeEditOverviewForm', [
  'pageService', '$routeParams',
  function (page, $routeParams) {
    var link = function ($scope) {
      var volume = $scope.volume;
      var form = $scope.volumeEditOverviewForm;

      function init(vol) {
        var volume = vol || {};
        var citation = volume.citation || {};
        form.data = {
          name: volume.name,
          alias: volume.alias,
          body: volume.body,
          citation: {
            head: citation.head,
            url: citation.url,
            year: citation.year
          },
          published: vol && vol.citation != null
        };
      }
      init(volume);

      form.save = function () {
        page.messages.clear(form);
        if (!form.data.published)
          form.data.citation = {head:''};

        (volume ?
          volume.save(form.data) :
          page.models.Volume.create(form.data, $routeParams.owner))
          .then(function (vol) {
            form.validator.server({});

            page.messages.add({
              type: 'green',
              body: page.constants.message('volume.edit.success'),
              owner: form
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
        page.messages.clear(form);
        var doi = page.constants.regex.doi.exec(form.data.citation.url);

        if (!doi || !doi[1]) {
          return;
        }

        page.models.cite(doi[1])
          .then(function (res) {
            form.data.name = res.title;
            form.data.citation = res;
            delete res.title;

            form.$setDirty();

            page.messages.add({
              type: 'green',
              body: page.constants.message('volume.edit.autodoi.citation.success'),
              owner: form
            });
          }, function () {
            page.messages.add({
              type: 'red',
              body: page.constants.message('volume.edit.autodoi.citation.error'),
              owner: form
            });
          });
      };

      var validate = {};
      ['name', 'body', 'alias', 'citation.head', 'citation.url', 'citation.year'].forEach(function (f) {
        validate[f] = {
          tips: page.constants.message('volume.edit.' + f + '.help')
        };
      });
      form.validator.client(validate, true);
    };

    return {
      restrict: 'E',
      templateUrl: 'volume/editOverview.html',
      link: link
    };
  }
]);
