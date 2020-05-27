function sOutBS = makeTestStructByteStream()
  sOut = makeTestStruct();
  sOutBS = getByteStreamFromArray(sOut);
end
