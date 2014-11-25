'use strict'

app.factory('Store', [
  '$sce', 'constantService', 'routerService', 'messageService', 'modelService',
  ($sce, constants, router, messages, models) -> class Store
    constructor: (@slot, asset) ->
      if asset
        @setAsset(asset)
      else
        @fillData()

    setAsset: (@asset) ->
      @fillData()

    fillData: ->
      @data =
        if @asset
          name: @asset.name
          classification: @asset.classification+''
          container: @slot.id # required for position, which has the side-effect of restoring deleted/moved assets
          position: @asset.segment.l
          excerptOn: @asset.excerpt?
          excerpt: (@asset.excerpt || 0)+''
        else
          classification: constants.classification.RESTRICTED+''

    Object.defineProperty @prototype, 'name',
      get: ->
        return constants.message('asset.add') unless @file || @asset
        @asset?.name ? @data.name ? @file?.file.name ? constants.message('file')

    remove: ->
      return if @pending # sorry
      return unless confirm constants.message 'asset.remove.confirm'
      if @file
        @file.cancel()
        delete @file
        return true
      @asset.remove().then(=>
          messages.add
            type: 'green'
            countdown: 3000
            body: constants.message('asset.remove.success', @name)
          delete @asset
          true
        , (res) ->
          messages.addError
            type: 'red'
            body: constants.message('asset.remove.error', @name)
            report: res
          false
        )

    save: ->
      return if @pending # sorry
      @pending = 1
      @data.excerpt = '' if 'excerpt' of @data && !@data.excerptOn
      (if @file
        @data.upload = @file.uniqueIdentifier
        if @asset then @asset.replace(@data) else @slot.createAsset(@data)
      else
        @asset.save(@data)
      ).then((asset) =>
          delete @pending

          first = !@asset
          if asset instanceof models.Asset
            @asset.asset = asset
            asset = @asset
          @setAsset(asset)

          messages.add
            type: 'green'
            countdown: 3000
            body: constants.message('asset.' + (if @file then (if first then 'upload' else 'replace') else 'update') + '.success', @name) +
              (if @file && asset.format.transcodable then ' ' + constants.message('asset.upload.transcoding') else '')

          if @file
            unless 'creation' of asset.asset
              asset.asset.creation = {date: Date.now(), name: @file.file.name}
            @file.cancel()
            delete @file
            delete @progress
          true
        , (res) =>
          delete @pending
          messages.addError
            type: 'red'
            body: constants.message('asset.update.error', @name)
            report: res
          if @file
            @file.cancel()
            delete @file
            delete @progress
            delete @data.upload
          false
        )

    upload: (file) ->
      return if @file
      file.pause()
      @file = file
      @progress = 0
      file.store = this
      
      router.http(router.controllers.AssetApi.uploadStart, @slot.volume.id,
          filename: file.name
          size: file.size
        ).then((res) =>
          file.uniqueIdentifier = res.data
          file.resume()
          true
        , (res) =>
          messages.addError
            type: 'red'
            body: constants.message('asset.upload.rejected', {sce:$sce.HTML}, @name)
            report: res
          file.cancel()
          delete @file
          delete @progress
          false
        )

    excerptOptions: () ->
      l = {}
      for c, i in constants.classification when i == 0 || i > @data.classification
        l[i] = c
      l

    # callbacks for ng-flow:

    @fileSuccess = (file) ->
      file.store.progress = 1
      file.store.save()

    @fileProgress = (file) ->
      file.store.progress = file.progress()

    @flowOptions =
      target: router.controllers.AssetApi.uploadChunk().url
      method: 'octet'
      simultaneousUploads: 3
      testChunks: false
      chunkRetryInterval: 5000
      permanentErrors: [400, 403, 404, 409, 415, 500, 501]
      progressCallbacksInterval: 500
      prioritizeFirstAndLastChunk: true
])
