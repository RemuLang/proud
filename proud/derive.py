import typing as t
Pred =  t.Callable[[type, str,t.Union[str, type]], bool]


def pre_visitor(cls: t.Union[type, Pred], by: Pred = None):
    by = by or (lambda cls, _, ann: cls.__name__ == ann
                if isinstance(ann, str) else issubclass(ann, cls))
    if isinstance(cls, type):
        anns : t.Optional[t.Dict[str,t.Union[str, type]]] = getattr(cls, '__annotations__', None)
        access_fields = [k for k, ann in anns.items() if by(cls, k, ann)]
        if not anns:
            def pre_visit(self, f):
                return
        else:

            def pre_visit(self, f):
                for attr in access_fields:
                    field = getattr(self, attr)
                    f(field)
                    field.pre_visit(f)

        cls.pre_visit = pre_visit
        return cls
    by = cls
    return lambda cls: pre_visitor(cls, by)


def post_visitor(cls: t.Union[type, Pred], by: Pred = None):
    by = by or (lambda cls, _, ann: cls.__name__ == ann
                if isinstance(ann, str) else issubclass(ann, cls))
    if isinstance(cls, type):
        anns : t.Optional[t.Dict[str,t.Union[str, type]]] = getattr(cls, '__annotations__', None)
        access_fields = [k for k, ann in anns.items() if by(cls, k, ann)]
        if not anns:
            def post_visit(self, f):
                return
        else:
            def post_visit(self, f):
                for attr in access_fields:
                    field = getattr(self, attr)
                    field.post_visit(f)
                    f(field)


        cls.post_visit = post_visit
        return cls
    by = cls
    return lambda cls: post_visitor(cls, by)
