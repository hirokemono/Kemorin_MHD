!>@file   t_SGS_control_parameter.f90
!!@brief  module t_SGS_control_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief control flags for MHD dynamo model
!!
!!@verbatim
!!      subroutine alloc_filter_group_param(num_grp, f_area)
!!      subroutine dealloc_SGS_filter_groups(filter_param)
!!      subroutine dealloc_filter_group_param(f_area)
!!
!!      subroutine copy_filter_group_param(f_area_org, f_area_new)
!!        type(SGS_filter_area_params), intent(in) :: f_area_org
!!        type(SGS_filter_area_params), intent(inout) :: f_area_new
!!      integer(kind = kint) function dynamic_SGS_flag(i_step, SGS_par)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!@endverbatim
!
      module t_SGS_control_parameter
!
      use m_precision
      use m_constants
      use t_IO_step_parameter
!
      implicit  none
!
!
      integer (kind=kint), parameter :: id_SGS_none =       0
      integer (kind=kint), parameter :: id_SGS_NL_grad =    1
      integer (kind=kint), parameter :: id_SGS_similarity = 2
      integer (kind=kint), parameter :: id_SGS_diffusion =  3
!
      integer (kind=kint), parameter :: id_SGS_DYNAMIC_OFF =   0
      integer (kind=kint), parameter :: id_SGS_DYNAMIC_ON =    1
!
      integer (kind=kint), parameter :: id_CSIM_FIELD =        0
      integer (kind=kint), parameter :: id_CSIM_COMPONENT =    1
!
      integer (kind=kint), parameter :: id_SGS_REFERENCE =   1
      integer (kind=kint), parameter :: id_SGS_REF_AVERAGE = 2
!
      integer (kind=kint), parameter :: id_SGS_NO_CLIP =       0
      integer (kind=kint), parameter :: id_SGS_ZERO_CLIP =     1
      integer (kind=kint), parameter :: id_SGS_KEEP_PREVIOUS = 2
!
      integer (kind=kint), parameter :: id_SGS_DIR_LSQ =       0
      integer (kind=kint), parameter :: id_SGS_DIR_AVERAGE =   1
      integer (kind=kint), parameter :: id_SGS_DIR_CORRELATE = 2
!
      integer (kind=kint), parameter :: id_use_volume =   0
      integer (kind=kint), parameter :: id_use_zonal =    1
      integer (kind=kint), parameter :: id_use_sphere =   2
!
!>      filter ID for @f$ s\Delta @f$  filter
      integer (kind=kint), parameter :: ifilter_2delta = 1
!>      filter ID for @f$ 4\Delta @f$  filter
      integer (kind=kint), parameter :: ifilter_4delta = 2
!
!>      Parameters for SGS model
      type SGS_model_control_params
        integer (kind=kint) :: iflag_SGS =     id_SGS_none
        integer (kind=kint) :: iflag_dynamic = id_SGS_DYNAMIC_OFF
!
        integer (kind=kint) :: iflag_SGS_h_flux =  id_SGS_none
        integer (kind=kint) :: iflag_SGS_m_flux =  id_SGS_none
        integer (kind=kint) :: iflag_SGS_lorentz = id_SGS_none
        integer (kind=kint) :: iflag_SGS_uxb =     id_SGS_none
        integer (kind=kint) :: iflag_SGS_c_flux =  id_SGS_none
        integer (kind=kint) :: iflag_SGS_gravity = id_SGS_none
!
        real(kind = kreal) :: SGS_hf_factor =      1.0d0
        real(kind = kreal) :: SGS_mf_factor =      1.0d0
        real(kind = kreal) :: SGS_mawell_factor =  1.0d0
        real(kind = kreal) :: SGS_uxb_factor =     1.0d0
        real(kind = kreal) :: SGS_cf_factor =      1.0d0
!
        integer (kind=kint) :: min_step_dynamic =  1
        integer (kind=kint) :: max_step_dynamic =  1
        real (kind = kreal) :: shrink_SGS_dt = 1.0d5
        real (kind = kreal) :: extend_SGS_dt = 1.0d-5
!
!>        Model coefficient type
        integer (kind=kint) :: itype_Csym =  id_CSIM_FIELD
!>        Direction to evaluate model coefficients
        integer (kind=kint) :: icoord_Csim = 0
!>        Usae of model coefficients for SGS buoyancy
        integer (kind=kint) :: iflag_SGS_buo_usage = 0
!
        integer (kind=kint) :: itype_Csym_h_flux =   id_CSIM_FIELD
        integer (kind=kint) :: itype_Csym_c_flux =   id_CSIM_FIELD
        integer (kind=kint) :: itype_Csym_m_flux =   id_CSIM_FIELD
        integer (kind=kint) :: itype_Csym_maxwell =  id_CSIM_FIELD
        integer (kind=kint) :: itype_Csym_uxb =      id_CSIM_FIELD
!
!>        Flag to treat perturbation of SGSheat flux
        integer (kind=kint) :: iflag_parterbuation = id_turn_OFF
!
!>        fla to clip negative model coefficients
        integer (kind=kint) :: iflag_nagetive_clip = id_SGS_NO_CLIP
!>        Merging mode for model coefficients in direction
        integer (kind=kint) :: iflag_Csim_marging =  id_SGS_DIR_LSQ
!>        maximum value to clip model coefficients
        real (kind = kreal) :: clipping_limit = 0.0d0
!
!>        filter ID to obtain SGS terms
        integer (kind=kint) :: ifilter_final = ifilter_2delta
      end type SGS_model_control_params
!
!>      ID not to apply commutation error correction
      integer (kind=kint), parameter :: id_SGS_commute_OFF = 0
!>      ID to apply commutation error correction
      integer (kind=kint), parameter :: id_SGS_commute_ON =  1
!
!>      Parameters for ommutation error correction
      type commutation_control_params
        integer (kind=kint) :: iset_DIFF_coefs =  0
!
!>      commutation error correction flag for system
        integer (kind=kint) :: iflag_commute = id_SGS_commute_OFF
!>      commutation error correction flag for linear terms
        integer (kind=kint) :: iflag_c_linear = id_SGS_commute_OFF
!>      commutation error correction flag for nonlinear terms
        integer (kind=kint) :: iflag_c_nonlinars = id_SGS_commute_OFF
!
!>      commutation error correction flag for temperature
        integer (kind=kint) :: iflag_c_temp = id_SGS_commute_OFF
!>      commutation error correction flag for velocity
        integer (kind=kint) :: iflag_c_velo = id_SGS_commute_OFF
!>      commutation error correction flag for magnetic field
        integer (kind=kint) :: iflag_c_magne = id_SGS_commute_OFF
!>      commutation error correction flag for composition variation
        integer (kind=kint) :: iflag_c_light = id_SGS_commute_OFF
!
!>      commutation error correction flag for heat flux
        integer (kind=kint) :: iflag_c_hf = id_SGS_commute_OFF
!>      commutation error correction flag for momentum flux
        integer (kind=kint) :: iflag_c_mf = id_SGS_commute_OFF
!>      commutation error correction flag for heat flux
        integer (kind=kint) :: iflag_c_lorentz = id_SGS_commute_OFF
!>      commutation error correction flag for magnetic induction
        integer (kind=kint) :: iflag_c_uxb = id_SGS_commute_OFF
!>      commutation error correction flag for composition flux
        integer (kind=kint) :: iflag_c_cf  = id_SGS_commute_OFF
      end type commutation_control_params
!
!
      integer (kind=kint), parameter :: id_SGS_NO_FILTERING =         0
      integer (kind=kint), parameter :: id_SGS_3D_FILTERING =         1
      integer (kind=kint), parameter :: id_SGS_3D_EZ_FILTERING =     11
      integer (kind=kint), parameter :: id_SGS_3D_SMP_FILTERING =    21
      integer (kind=kint), parameter :: id_SGS_3D_EZ_SMP_FILTERING = 31
!
      integer (kind=kint), parameter :: id_SGS_LINE_FILTERING =       2
      integer (kind=kint), parameter :: id_SGS_PLANE_FILTERING =      3
      integer (kind=kint), parameter :: id_SGS_IDEAL_SPH_LOWPASS =    4
!
!
      type SGS_filter_area_params
        integer (kind=kint) :: num_f_group = 0
        integer (kind=kint), allocatable :: id_f_group(:)
        character (len=kchara), allocatable :: f_gourp_name(:)
      end type SGS_filter_area_params
!
!
!>      Structure for contriol parameters for filtering
      type SGS_filtering_params
        type(SGS_filter_area_params) :: whole
        type(SGS_filter_area_params) :: fluid
!
        type(SGS_filter_area_params) :: whole_wide
        type(SGS_filter_area_params) :: fluid_wide
!
        integer (kind=kint) :: iflag_heat_filtering =        0
        integer (kind=kint) :: iflag_composition_filtering = 0
        integer (kind=kint) :: iflag_momentum_filtering =    0
        integer (kind=kint) :: iflag_induction_filtering =   0
!
        integer (kind=kint) :: iflag_SGS_filter = id_SGS_3D_FILTERING
      end type SGS_filtering_params
!
!
      type SGS_paremeters
!>        Increment of time step for evaluation of SGS model coefficients
        integer(kind=kint) :: i_step_sgs_coefs = ione
!>        Flag for initial step for SGS model
        integer(kind=kint) :: iflag_SGS_initial =  1
!
        type(IO_step_param) :: sgs_step
!
!>        Parameters for SGS model
        type(SGS_model_control_params) :: model_p
!>        Parameters for commutation error correction
        type(commutation_control_params) :: commute_p
!>        Structure for contriol parameters for filtering
        type(SGS_filtering_params) :: filter_p
      end type SGS_paremeters
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_filter_group_param(num_grp, f_area)
!
      integer(kind = kint), intent(in) :: num_grp
      type(SGS_filter_area_params), intent(inout) :: f_area
!
!
      f_area%num_f_group = num_grp
      allocate(f_area%f_gourp_name(f_area%num_f_group))
      allocate(f_area%id_f_group(f_area%num_f_group))
      if(f_area%num_f_group .gt. 0) f_area%id_f_group = 0
!
      end subroutine alloc_filter_group_param
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_SGS_filter_groups(filter_param)
!
      type(SGS_filtering_params), intent(inout) :: filter_param
!
!
      call dealloc_filter_group_param(filter_param%whole)
      call dealloc_filter_group_param(filter_param%fluid_wide)
!
      call dealloc_filter_group_param(filter_param%whole_wide)
      call dealloc_filter_group_param(filter_param%fluid_wide)
!
      end subroutine dealloc_SGS_filter_groups
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_filter_group_param(f_area)
!
      type(SGS_filter_area_params), intent(inout) :: f_area
!
!
      deallocate(f_area%f_gourp_name, f_area%id_f_group)
!
      end subroutine dealloc_filter_group_param
!
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_group_param(f_area_org, f_area_new)
!
      type(SGS_filter_area_params), intent(in) :: f_area_org
      type(SGS_filter_area_params), intent(inout) :: f_area_new
!
!
      call alloc_filter_group_param(f_area_org%num_f_group, f_area_new)
      if(f_area_new%num_f_group .gt. 0) then
        f_area_new%f_gourp_name(1:f_area_new%num_f_group)               &
     &        = f_area_org%f_gourp_name(1:f_area_new%num_f_group)
        f_area_new%id_f_group(1:f_area_new%num_f_group)                 &
     &        = f_area_org%id_f_group(1:f_area_new%num_f_group)
      end if
!
      end subroutine copy_filter_group_param
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function dynamic_SGS_flag(i_step, SGS_par)
!
      use t_IO_step_parameter
!
      integer (kind =kint), intent(in) :: i_step
      type(SGS_paremeters), intent(in) :: SGS_par
!
!
      dynamic_SGS_flag = output_flag(i_step, SGS_par%i_step_sgs_coefs)
!
      end function dynamic_SGS_flag
!
!-----------------------------------------------------------------------
!
      end module t_SGS_control_parameter
