### v0.4.0 2022-01-25 (Paris)

- Split the distribution into two packages: `multipart_form{,-lwt}` (@dinosaure, @leviroth, #23)
- Documentation about `Content-Type` (@dinosaure, #24)
- Upgrade the distribution with few new releases (@dinosaure, #24)
- Add a test about the lwt support (@dinosaure, @taiseiKMC, #25, #27)
- Allow filenames with space (@dinosaure, @aantron, @cemerick, #28)

### v0.3.0 2021-05-11 (Paris)

- Add some `of_string`/`to_string` functions (@dinosaure, #15)
- Add tests about content-type parser (@dinosaure, #16)
- Use `prettym` package instead of `mrmime.prettym` (@dinosaure, #17)
- Fix infinite loop for large upload (@dinosaure, #18)

### v0.2.0 2021-04-19 (Paris)

- Add an encoder of `multipart/form-data` (@dinosaure, #9)
- Provide `multipart_form.lwt` which is a specialization
  of `multipart_form` with `lwt` and _streams_
  (@Armael, @dinosaure, #10)
- Add convenience functions such as `map` or `flatten`
  (@Armael, #10)
- Add `Multipart_form.parse` (@Armael, #10)
- Add documentation to help user (@Armael, @dinosaure, #10, #11, #13)
- Update the README.md (@dinosaure, #12)

### v0.1.0 2020-09-14 (Paris)

- First release of `multipart_form`
