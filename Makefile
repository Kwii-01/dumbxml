NAME	= 	schemaInferrer

BUILD	=	stack build

RM	=	rm -rf

all:	$(NAME)

$(NAME):
		$(BUILD)
		stack install --local-bin-path .
		mv dumbXML-exe $(NAME)

clean: 

fclean:
		$(RM) $(NAME)

re:	fclean all

.PHONY:	all clean fclean re