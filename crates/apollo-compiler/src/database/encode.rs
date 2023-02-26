use super::hir;
use apollo_encoder as encoder;

pub enum IntoEncoderError {
    FloatCoercionError,
}

impl TryInto<encoder::ObjectDefinition> for &hir::ObjectTypeDefinition {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::ObjectDefinition, Self::Error> {
        let name = self.name().to_owned();
        let mut def = encoder::ObjectDefinition::new(name);

        if let Some(description) = self.description.clone() {
            def.description(description);
        }

        for interface in self.implements_interfaces() {
            def.interface(interface.interface().to_owned());
        }

        for field in self.fields_definition() {
            def.field(field.try_into()?);
        }

        for directive in self.directives() {
            def.directive(directive.try_into()?);
        }

        Ok(def)
    }
}

impl TryInto<encoder::InterfaceDefinition> for &hir::InterfaceTypeDefinition {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::InterfaceDefinition, Self::Error> {
        let name = self.name().to_owned();
        let mut def = encoder::InterfaceDefinition::new(name);

        if let Some(description) = self.description.clone() {
            def.description(description);
        }

        for interface in self.implements_interfaces() {
            def.interface(interface.interface().to_owned());
        }

        for field in self.fields_definition() {
            def.field(field.try_into()?);
        }

        for directive in self.directives() {
            def.directive(directive.try_into()?);
        }

        Ok(def)
    }
}

impl TryInto<encoder::ScalarDefinition> for &hir::ScalarTypeDefinition {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::ScalarDefinition, Self::Error> {
        let name = self.name().to_owned();
        let mut def = encoder::ScalarDefinition::new(name);

        if let Some(description) = self.description.clone() {
            def.description(description);
        }

        for directive in self.directives() {
            def.directive(directive.try_into()?);
        }

        Ok(def)
    }
}

impl TryInto<encoder::UnionDefinition> for &hir::UnionTypeDefinition {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::UnionDefinition, Self::Error> {
        let name = self.name().to_owned();
        let mut def = encoder::UnionDefinition::new(name);

        if let Some(description) = self.description.clone() {
            def.description(description);
        }

        for member in self.union_members() {
            def.member(member.name().to_owned());
        }

        for directive in self.directives() {
            def.directive(directive.try_into()?);
        }

        Ok(def)
    }
}

impl TryInto<encoder::EnumDefinition> for &hir::EnumTypeDefinition {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::EnumDefinition, Self::Error> {
        let name = self.name().to_owned();
        let mut def = encoder::EnumDefinition::new(name);

        for value in self.enum_values_definition() {
            def.value(value.try_into()?);
        }

        for directive in self.directives() {
            def.directive(directive.try_into()?);
        }

        Ok(def)
    }
}

impl TryInto<encoder::EnumValue> for &hir::EnumValueDefinition {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::EnumValue, Self::Error> {
        let enum_value = self.enum_value().to_owned();
        let mut def = encoder::EnumValue::new(enum_value);

        if let Some(description) = self.description.clone() {
            def.description(description);
        }

        for directive in self.directives() {
            def.directive(directive.try_into()?);
        }

        Ok(def)
    }
}

impl TryInto<encoder::InputObjectDefinition> for &hir::InputObjectTypeDefinition {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::InputObjectDefinition, Self::Error> {
        let name = self.name().to_owned();
        let mut def = encoder::InputObjectDefinition::new(name);

        if let Some(description) = self.description.clone() {
            def.description(description);
        }

        for directive in self.directives() {
            def.directive(directive.try_into()?);
        }

        for input_field in self.input_fields_definition() {
            def.field(input_field.try_into()?);
        }

        Ok(def)
    }
}

impl TryInto<encoder::InputField> for &hir::InputValueDefinition {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::InputField, Self::Error> {
        let name = self.name().to_owned();
        let typ = self.ty().try_into()?;
        let mut def = encoder::InputField::new(name, typ);

        if let Some(description) = self.description.clone() {
            def.description(description);
        }

        for directive in self.directives() {
            def.directive(directive.try_into()?);
        }

        if let Some(default_value) = self.default_value() {
            let encoder_value: encoder::Value = default_value.try_into()?;
            let value_str = format!("{}", encoder_value); //TODO verify this
            def.default_value(value_str);
        }

        Ok(def)
    }
}

impl TryInto<encoder::FieldDefinition> for &hir::FieldDefinition {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::FieldDefinition, Self::Error> {
        let name = self.name().to_owned();
        let field_type = self.ty().try_into()?;
        let mut def = encoder::FieldDefinition::new(name, field_type);

        if let Some(description) = self.description.clone() {
            def.description(description);
        }

        for iv_def in self.arguments().input_values() {
            def.arg(iv_def.try_into()?);
        }

        for directive in self.directives() {
            def.directive(directive.try_into()?);
        }

        Ok(def)
    }
}

impl TryInto<encoder::Type_> for &hir::Type {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::Type_, Self::Error> {
        let ty = match self {
            hir::Type::NonNull { ty: hir_ty, .. } => encoder::Type_::NonNull {
                ty: Box::new(hir_ty.as_ref().try_into()?),
            },
            hir::Type::List { ty: hir_ty, .. } => encoder::Type_::List {
                ty: Box::new(hir_ty.as_ref().try_into()?),
            },
            hir::Type::Named { name, .. } => encoder::Type_::NamedType {
                name: name.to_owned(),
            },
        };

        Ok(ty)
    }
}

impl TryInto<encoder::Directive> for &hir::Directive {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::Directive, Self::Error> {
        let name = self.name().to_owned();
        let mut directive = encoder::Directive::new(name);

        for arg in self.arguments() {
            directive.arg(arg.try_into()?);
        }

        Ok(directive)
    }
}

impl TryInto<encoder::Argument> for &hir::Argument {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::Argument, Self::Error> {
        let name = self.name().to_owned();
        let value = self.value().try_into()?;
        let arg = encoder::Argument::new(name, value);

        Ok(arg)
    }
}

impl TryInto<encoder::Value> for &hir::Value {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::Value, Self::Error> {
        let value = match self {
            hir::Value::Variable(v) => encoder::Value::Variable(v.name.to_owned()),

            //TODO look more closely at int conversion
            hir::Value::Int(i) => encoder::Value::Int(
                i.to_i32_checked()
                    .ok_or(IntoEncoderError::FloatCoercionError)?,
            ),
            hir::Value::Float(f) => encoder::Value::Float(f.get()),
            hir::Value::String(s) => encoder::Value::String(s.clone()),
            hir::Value::Boolean(b) => encoder::Value::Boolean(*b),
            hir::Value::Null => encoder::Value::Null,
            hir::Value::Enum(e) => encoder::Value::Enum(e.src().to_owned()),
            hir::Value::List(l) => encoder::Value::List(
                l.iter()
                    .map(TryInto::<encoder::Value>::try_into)
                    .collect::<Result<Vec<_>, IntoEncoderError>>()?,
            ),
            hir::Value::Object(fields) => encoder::Value::Object(
                fields
                    .iter()
                    .map(|(n, v)| {
                        v.try_into()
                            .map(|v: encoder::Value| (n.src().to_owned(), v))
                    })
                    .collect::<Result<Vec<_>, IntoEncoderError>>()?,
            ),
        };

        Ok(value)
    }
}

impl TryInto<encoder::InputValueDefinition> for &hir::InputValueDefinition {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::InputValueDefinition, Self::Error> {
        let name = self.name().to_owned();
        let iv_type = self.ty().try_into()?;
        let mut def = encoder::InputValueDefinition::new(name, iv_type);

        if let Some(description) = self.description.clone() {
            def.description(description);
        }

        for directive in self.directives() {
            def.directive(directive.try_into()?);
        }

        if let Some(default_value) = self.default_value() {
            let encoder_value: encoder::Value = default_value.try_into()?;
            let value_str = format!("{}", encoder_value); //TODO verify this
            def.default_value(value_str);
        }

        Ok(def)
    }
}

impl TryInto<encoder::DirectiveDefinition> for &hir::DirectiveDefinition {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::DirectiveDefinition, Self::Error> {
        let name = self.name().to_owned();
        let mut def = encoder::DirectiveDefinition::new(name);

        if let Some(description) = self.description.clone() {
            def.description(description);
        }

        if self.repeatable() {
            def.repeatable();
        }

        for arg in self.arguments().input_values() {
            def.arg(arg.try_into()?);
        }

        for directive_loc in self.directive_locations() {
            def.location(directive_loc.name().to_owned());
        }

        Ok(def)
    }
}

impl TryInto<encoder::FragmentDefinition> for &hir::FragmentDefinition {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::FragmentDefinition, Self::Error> {
        let name = self.name().to_owned();
        let type_cond = self.type_condition().to_owned();
        let selection_set = self.selection_set().try_into()?;

        let def = encoder::FragmentDefinition::new(
            name,
            encoder::TypeCondition::new(type_cond),
            selection_set,
        );

        Ok(def)
    }
}

impl TryInto<encoder::SelectionSet> for &hir::SelectionSet {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::SelectionSet, Self::Error> {
        let mut selection_set = encoder::SelectionSet::new();

        for selection in self.selection() {
            selection_set.selection(selection.try_into()?)
        }

        Ok(selection_set)
    }
}

impl TryInto<encoder::Selection> for &hir::Selection {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::Selection, Self::Error> {
        let selection = match self {
            hir::Selection::Field(field) => encoder::Selection::Field(field.as_ref().try_into()?),
            hir::Selection::FragmentSpread(fragment) => {
                encoder::Selection::FragmentSpread(fragment.as_ref().try_into()?)
            }
            hir::Selection::InlineFragment(fragment) => {
                encoder::Selection::InlineFragment(fragment.as_ref().try_into()?)
            }
        };

        Ok(selection)
    }
}

impl TryInto<encoder::Field> for &hir::Field {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::Field, Self::Error> {
        let name = self.name().to_owned();
        let mut field = encoder::Field::new(name);

        field.alias(self.alias().map(|a| a.0.clone()));

        for arg in self.arguments() {
            field.argument(arg.try_into()?);
        }

        for directive in self.directives() {
            field.directive(directive.try_into()?);
        }

        if !self.selection_set().selection().is_empty() {
            field.selection_set(Some(self.selection_set().try_into()?));
        }

        Ok(field)
    }
}

impl TryInto<encoder::FragmentSpread> for &hir::FragmentSpread {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::FragmentSpread, Self::Error> {
        let name = self.name().to_owned();
        let mut fragment = encoder::FragmentSpread::new(name);

        for directive in self.directives() {
            fragment.directive(directive.try_into()?);
        }

        Ok(fragment)
    }
}

impl TryInto<encoder::InlineFragment> for &hir::InlineFragment {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::InlineFragment, Self::Error> {
        let selection_set = self.selection_set().try_into()?;
        let mut fragment = encoder::InlineFragment::new(selection_set);

        fragment.type_condition(
            self.type_condition()
                .map(|tc| encoder::TypeCondition::new(tc.to_owned())),
        );

        for directive in self.directives() {
            fragment.directive(directive.try_into()?);
        }

        Ok(fragment)
    }
}

impl TryInto<encoder::VariableDefinition> for &hir::VariableDefinition {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::VariableDefinition, Self::Error> {
        let name = self.name().to_owned();
        let ty = self.ty().try_into()?;
        let mut def = encoder::VariableDefinition::new(name, ty);

        if let Some(default_value) = self.default_value() {
            def.default_value(default_value.try_into()?);
        }

        Ok(def)
    }
}

impl TryInto<encoder::OperationDefinition> for &hir::OperationDefinition {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::OperationDefinition, Self::Error> {
        let operation_type = self.operation_ty().try_into()?;
        let selection_set = self.selection_set().try_into()?;

        let mut def = encoder::OperationDefinition::new(operation_type, selection_set);

        for directive in self.directives() {
            def.directive(directive.try_into()?);
        }

        for var in self.variables() {
            def.variable_definition(var.try_into()?);
        }

        Ok(def)
    }
}

impl TryInto<encoder::OperationType> for hir::OperationType {
    type Error = IntoEncoderError;

    fn try_into(self) -> Result<encoder::OperationType, Self::Error> {
        Ok(match self {
            hir::OperationType::Query => encoder::OperationType::Query,
            hir::OperationType::Mutation => encoder::OperationType::Mutation,
            hir::OperationType::Subscription => encoder::OperationType::Subscription,
        })
    }
}
