# load package
#devtools::load_all()

# test object returned from function
test_that("function pulls expected data data from bgg", {

        # use function
        collection = get_user_collection('mrbananagrabber')

        # defined collection names that should appear
        collection_names = c("username",
                             "url",
                             "load_ts",
                             "game_id",
                             "name",
                             "type",
                             "rating",
                             "own",
                             "preordered",
                             "prevowned",
                             "fortrade",
                             "want",
                             "wanttoplay",
                             "wanttobuy",
                             "wishlist",
                             "wishlistpriority")

        # did function return a list?
        expect_type(collection, type = "list")
        # did function return records?
        expect_gt(nrow(collection), 0)
        # did the names match expected?
        expect_identical(collection_names, names(collection))

})

