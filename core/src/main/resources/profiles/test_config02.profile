deidentiFHIR.profile.version=0.2
modules = {
    # TODO include modules
    laboratory: {include required("test_config02_module1.conf")}
    person: {include required("test_config02_module2.conf")}
}
