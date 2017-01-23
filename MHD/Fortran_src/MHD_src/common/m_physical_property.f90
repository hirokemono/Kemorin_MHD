!>@file   m_physical_property.f90
!!@brief  module m_physical_property
!!
!!@author H. Matsui
!!@date Programmed in 2001
!!@date Modified in Jan., 2007
!
!>@brief  Coeffiecients of each term
!
      module m_physical_property
!
      use m_precision
      use t_physical_property
      use t_reference_scalar_param
!
      implicit  none
!
!
!>      Structure for fluid property
      type(fluid_property), save :: fl_prop1
!>      Structure for manetic property
      type(conductive_property), save :: cd_prop1
!
!>      Structure for thermal property
      type(scalar_property), save :: ht_prop1
!>      Structure for compositon property
      type(scalar_property), save :: cp_prop1
!
!>      Takepiro stratified temperature
      type(takepiro_model_param), save :: takepito_T1
!>      Takepiro stratified composition
      type(takepiro_model_param), save :: takepito_C1
!
      end module m_physical_property
