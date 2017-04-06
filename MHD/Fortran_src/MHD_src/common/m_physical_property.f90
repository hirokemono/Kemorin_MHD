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
      use t_control_parameter
!
      implicit  none
!
!
      type(MHD_evolution_param), save :: MHD_prop1
!MHD_prop1%fl_prop, MHD_prop1%cd_prop, MHD_prop1%ht_prop, MHD_prop1%cp_prop
!
      integer (kind=kint) :: iflag_scheme = id_Crank_nicolson
!
!>      reference paramter for temperature
      type(reference_scalar_param), save :: ref_param_T1
!>      Takepiro stratified composition
      type(reference_scalar_param), save :: ref_param_C1
!
!>      Takepiro stratified temperature
      type(takepiro_model_param), save :: takepito_T1
!>      Takepiro stratified composition
      type(takepiro_model_param), save :: takepito_C1
!
      end module m_physical_property
