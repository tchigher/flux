from(db:"test")
    |> range(start:-5m)
    |> max() 