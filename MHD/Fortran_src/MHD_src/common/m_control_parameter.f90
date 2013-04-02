!
!     module   m_control_parameter
!.......................................................................
!
!      subroutine allocate_fluid_ele_grp_name
!      subroutine allocate_conduct_ele_grp_name
!      subroutine allocate_icore_ele_grp_name
!
!      subroutine deallocate_fluid_ele_grp_name
!      subroutine deallocate_conduct_ele_grp_name
!      subroutine deallocate_icore_ele_grp_name
!
!
      module   m_control_parameter
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint) :: iflag_sph_file = 0
!
      integer (kind=kint)  :: intg_point_t_evo
      integer (kind=kint)  :: intg_point_poisson
!
      integer (kind=kint)  :: num_multi_pass
! 
      integer (kind=kint) :: num_dimless
      character(len=kchara), allocatable :: name_dimless(:)
      real(kind=kreal), allocatable :: dimless(:)
!
!
      character(len=1)  :: detect_comment
!
!
      integer (kind=kint)  :: num_t_evo_control
      character (len=kchara), allocatable :: t_evo_name(:)
      character (len=kchara), allocatable :: time_evo_method(:)
!
      integer (kind=kint) :: num_fl_ele_grp
      integer (kind=kint) :: num_cd_ele_grp
      integer (kind=kint) :: num_ins_ele_grp
      integer (kind=kint) :: num_in_core_ele_grp
!
      character (len=kchara), allocatable :: fl_ele_grp_name(:)
      character (len=kchara), allocatable :: cd_ele_grp_name(:)
      character (len=kchara), allocatable :: ins_ele_grp_name(:)
      character (len=kchara), allocatable :: in_core_ele_grp_name(:)
!
!
      integer (kind=kint) :: iflag_t_evo_4_velo =     0
      integer (kind=kint) :: iflag_t_evo_4_temp =     0
      integer (kind=kint) :: iflag_t_evo_4_magne =    0
      integer (kind=kint) :: iflag_t_evo_4_vect_p =   0
      integer (kind=kint) :: iflag_t_evo_4_composit = 0
!
      integer (kind=kint) :: iflag_implicit_correct = 0
!
!
      integer (kind=kint) :: iflag_magneto_cv
!
!
       integer (kind=kint) :: iflag_4_supg
!
!
      integer (kind=kint) :: num_force
      character (len=kchara), allocatable :: name_force(:)
!
      integer (kind=kint) :: iflag_4_gravity =        0
      integer (kind=kint) :: iflag_4_coriolis =       0
      integer (kind=kint) :: iflag_4_lorentz =        0
      integer (kind=kint) :: iflag_4_composit_buo =   0
      integer (kind=kint) :: iflag_4_filter_gravity = 0
!
      integer (kind=kint) :: iflag_4_rotate
!
!
      integer (kind=kint) :: iflag_scheme
!
!
      integer (kind=kint) :: maxiter
      integer (kind=kint) :: maxiter_vecp
! 
      real (kind=kreal) :: eps_4_velo
      real (kind=kreal) :: eps_4_magne
!
      real (kind=kreal) :: rsig
!
!
      character (len=kchara) :: ordering_name
!
      integer (kind=kint), parameter :: id_SGS_none =       0
      integer (kind=kint), parameter :: id_SGS_NL_grad =    1
      integer (kind=kint), parameter :: id_SGS_similarity = 2
      integer (kind=kint), parameter :: id_SGS_diffusion =  3
      integer (kind=kint) :: iflag_SGS_model = id_SGS_none
!
      integer (kind=kint), parameter :: id_SGS_DYNAMIC_OFF =   0
      integer (kind=kint), parameter :: id_SGS_DYNAMIC_ON =    1
      integer (kind=kint) :: iflag_dynamic_SGS = id_SGS_DYNAMIC_OFF
!
      integer (kind=kint) :: iflag_SGS_filter =       1
      integer (kind=kint) :: iset_DIFF_model_coefs =  0
      integer (kind=kint) :: iset_SGS_nagetive_clip = 0
      integer (kind=kint) :: iset_SGS_coef_marging =  0
      real (kind = kreal) :: SGS_clipping_limit = 0.0d0
!
      real (kind = kreal) :: SGS_hf_factor =      1.0d0
      real (kind = kreal) :: SGS_mf_factor =      1.0d0
      real (kind = kreal) :: SGS_mawell_factor =  1.0d0
      real (kind = kreal) :: SGS_uxb_factor =     1.0d0
!
      integer (kind=kint) :: min_step_dynamic =  1
      integer (kind=kint) :: max_step_dynamic =  1
      real (kind = kreal) :: delta_to_shrink_dynamic = 1.0d5
      real (kind = kreal) :: delta_to_extend_dynamic = 1.0d-5
!
      integer (kind=kint) :: iflag_SGS_heat =      id_SGS_none
      integer (kind=kint) :: iflag_SGS_inertia =   id_SGS_none
      integer (kind=kint) :: iflag_SGS_lorentz =   id_SGS_none
      integer (kind=kint) :: iflag_SGS_induction = id_SGS_none
      integer (kind=kint) :: iflag_SGS_comp_flux = id_SGS_none
      integer (kind=kint) :: iflag_SGS_gravity =   id_SGS_none
!
      integer (kind=kint) :: iflag_SGS_parterbuation = 0
!
      integer (kind=kint) :: itype_SGS_model_coef =  0
      integer (kind=kint) :: icoord_SGS_model_coef = 0
!
      integer (kind=kint) :: itype_SGS_h_flux_coef =   0
      integer (kind=kint) :: itype_SGS_m_flux_coef =   0
      integer (kind=kint) :: itype_SGS_maxwell_coef =  0
      integer (kind=kint) :: itype_SGS_uxb_coef =      0
!
      integer (kind=kint), parameter :: id_SGS_commute_OFF = 0
      integer (kind=kint), parameter :: id_SGS_commute_ON =  1
!
      integer (kind=kint) :: iflag_commute_correction                   &
     &                      = id_SGS_commute_OFF
      integer (kind=kint) :: iflag_commute_linear                       &
     &                      = id_SGS_commute_OFF
      integer (kind=kint) :: iflag_commute_nonlinar                     &
     &                      = id_SGS_commute_OFF
!
      integer (kind=kint) :: iflag_commute_temp                         &
     &                      = id_SGS_commute_OFF
      integer (kind=kint) :: iflag_commute_velo                         &
     &                      = id_SGS_commute_OFF
      integer (kind=kint) :: iflag_commute_magne                        &
     &                      = id_SGS_commute_OFF
      integer (kind=kint) :: iflag_commute_composit                     &
     &                      = id_SGS_commute_OFF
!
      integer (kind=kint) :: iflag_commute_heat                         &
     &                      = id_SGS_commute_OFF
      integer (kind=kint) :: iflag_commute_inertia                      &
     &                      = id_SGS_commute_OFF
      integer (kind=kint) :: iflag_commute_lorentz                      &
     &                      = id_SGS_commute_OFF
      integer (kind=kint) :: iflag_commute_induction                    &
     &                      = id_SGS_commute_OFF
      integer (kind=kint) :: iflag_commute_c_flux                       &
     &                      = id_SGS_commute_OFF
!
      integer (kind=kint) :: num_whole_filter_grp = 0
      integer (kind=kint) :: num_fluid_filter_grp = 0
      integer (kind=kint), allocatable :: id_whole_filter_grp(:)
      integer (kind=kint), allocatable :: id_fluid_filter_grp(:)
      character (len=kchara), allocatable :: whole_filter_grp(:)
      character (len=kchara), allocatable :: fluid_filter_grp(:)
!
      integer (kind=kint) :: num_whole_w_filter_grp = 0
      integer (kind=kint) :: num_fluid_w_filter_grp = 0
      integer (kind=kint), allocatable :: id_whole_w_filter_grp(:)
      integer (kind=kint), allocatable :: id_fluid_w_filter_grp(:)
      character (len=kchara), allocatable :: whole_w_filter_grp(:)
      character (len=kchara), allocatable :: fluid_w_filter_grp(:)
!
      integer (kind=kint) :: iflag_heat_filtering = 0
      integer (kind=kint) :: iflag_momentum_filtering = 0
      integer (kind=kint) :: iflag_induction_filtering = 0
!
!
      integer (kind=kint) :: n_filter_final = 1
      integer (kind=kint) :: n_second = 1
      integer (kind=kint) :: n_quad = 2
!
      integer (kind=kint) :: iflag_4_ref_temp
      real (kind = kreal) :: low_temp
      real (kind = kreal) :: high_temp
      real (kind = kreal) :: depth_low_t
      real (kind = kreal) :: depth_high_t
!
      integer (kind=kint) :: iflag_straficate
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_fluid_ele_grp_name
!
      allocate(fl_ele_grp_name(num_fl_ele_grp))
!
      end subroutine allocate_fluid_ele_grp_name
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_conduct_ele_grp_name
!
      allocate(cd_ele_grp_name(num_cd_ele_grp))
!
      end subroutine allocate_conduct_ele_grp_name
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_icore_ele_grp_name
!
      allocate(in_core_ele_grp_name(num_in_core_ele_grp))
!
      end subroutine allocate_icore_ele_grp_name
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_fluid_ele_grp_name
!
      deallocate(fl_ele_grp_name)
!
      end subroutine deallocate_fluid_ele_grp_name
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_conduct_ele_grp_name
!
      deallocate(cd_ele_grp_name)
!
      end subroutine deallocate_conduct_ele_grp_name
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_icore_ele_grp_name
!
      deallocate(in_core_ele_grp_name)
!
      end subroutine deallocate_icore_ele_grp_name
!
!  ---------------------------------------------------------------------
!
      end module m_control_parameter
