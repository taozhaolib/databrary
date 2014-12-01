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
        data = form.data.filter (ref) ->
          !ref.removed && (ref.head != '' || ref.url != '')
        volume.save({links: data}).then(() ->
            form.validator.server {}
            form.data = data
            blank()

            form.messages.add
              type: 'green'
              countdown: 3000
              body: page.constants.message('volume.edit.success')

            form.$setPristine()
          , (res) ->
            form.validator.server res
          )

      return
]
