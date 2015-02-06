'use strict'

app.directive 'volumeEditLinksForm', [
  'pageService',
  (page) ->
    restrict: 'E',
    templateUrl: 'volume/editLinks.html',
    link: ($scope) ->
      volume = $scope.volume
      form = $scope.volumeEditLinksForm

      form.data = volume.links.map (ref) ->
        head: ref.head
        url: ref.url

      blank = () ->
        form.data.push
          head: ''
          url: ''
      blank()

      form.change = () ->
        if form.data[form.data.length-1].url != ''
          blank()

      form.remove = (ref) ->
        ref.removed = true
        ref.head = ''
        ref.url = ''
        form.$setDirty()

      form.save = () ->
        page.messages.clear(form)
        data = form.data.filter (ref) -> !ref.removed
        volume.save({links: data}).then(() ->
            form.validator.server {}
            form.data = data.filter (ref) -> ref.head != '' || ref.url != ''
            blank()

            page.messages.add
              type: 'green'
              body: page.constants.message('volume.edit.success')
              owner: form

            form.$setPristine()
          , (res) ->
            form.validator.server res
          )

      return
]
