rsconf = {
    _id: "shard_jasobrown",
    members: [
        {
            _id: 1,
            host: "127.0.0.1:9902"
        },
        {
            _id: 2,
            host: "127.0.0.2:9902"
        },
        {
            _id: 3,
            host: "127.0.0.3:9902"
        }
    ]
}
rs.initate(rsconf)
