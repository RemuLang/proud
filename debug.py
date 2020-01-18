from proud.pybackend import PyBackEnd

filename = "example_code/hkt.prd"
end = PyBackEnd()
end.verbose = True
# try:
end.main(filename, filename + '.sij')


