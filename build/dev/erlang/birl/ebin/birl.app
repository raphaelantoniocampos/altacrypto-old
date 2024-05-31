{application, birl, [
    {vsn, "1.7.0"},
    {applications, [gleam_stdlib,
                    ranger]},
    {description, "Date / Time handling for Gleam"},
    {modules, [birl,
               birl@duration,
               birl@interval,
               birl@zones]},
    {registered, []}
]}.
