!
!      module t_ctl_params_4_gen_filter
!
!     Written by H. Matsui on July, 2006
!
!!      subroutine alloc_moment_parameter(gfil_p)
!!      subroutine alloc_ref_filter_type(gfil_p)
!!      subroutine alloc_ref_filter_area(gfil_p)
!!
!!      subroutine dealloc_moment_parameter(gfil_p)
!!      subroutine dealloc_ref_filter_type(gfil_p)
!!        type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!
!
      module t_ctl_params_4_gen_filter
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint), parameter :: iflag_tophat_filter =   0
      integer(kind = kint), parameter :: iflag_linear_filter =   1
      integer(kind = kint), parameter :: iflag_gaussian_filter = 2
!
      integer(kind = kint), parameter :: iflag_no_filter =  -10
      integer(kind = kint), parameter :: iflag_undefined =    0
      integer(kind = kint), parameter :: iflag_commutative =  1
      integer(kind = kint), parameter :: iflag_tophat =  2
      integer(kind = kint), parameter :: iflag_linear =  3
      integer(kind = kint), parameter :: iflag_gaussian =  4
!
      type ctl_params_4_gen_filter
        integer(kind = kint) :: num_int_points
        integer(kind = kint) :: minimum_comp
        integer(kind = kint) :: maximum_neighbour = 1
        integer(kind = kint) :: ist_num_free = -1
        integer(kind = kint) :: ied_num_free = -1
        integer(kind = kint) :: iflag_tgt_filter_type = 0
        integer(kind = kint) :: iflag_momentum_type =   0
        integer(kind = kint) :: iflag_ordering_list =   0
        integer(kind = kint) :: iflag_negative_center = 1
        real(kind = kreal) :: omitted_ratio
        real(kind = kreal) ::  minimum_det_mat = 1.0d+01
        real(kind = kreal) ::  max_rms_weight_limit = 2.0d+00
!
        integer(kind = kint) :: iflag_err_level_filter = 0
        integer(kind = kint) :: iflag_use_fixed_points = 0
!
        integer(kind = kint) :: inod_start_filter = 1
        integer(kind = kint) :: inod_end_filter =  -1
!
        integer(kind = kint) :: num_filtering_grp
        integer(kind = kint), allocatable :: id_filter_area_grp(:)
        character(len = kchara), allocatable :: filter_area_name(:)
!
!
        integer(kind = kint) :: num_ref_filter
        integer(kind = kint), allocatable :: iref_filter_type(:)
        real(kind = kreal), allocatable :: ref_filter_width(:)
!
        integer(kind = kint) :: num_moments_order = 0
        integer(kind = kint), allocatable :: mom_order(:)
        real(kind = kreal), allocatable :: mom_value(:)
        integer(kind = kint), allocatable :: iref_mom_type(:)
!
!
        integer (kind=kint) :: id_solver_type
!
        integer (kind=kint) :: itr
        real (kind=kreal) :: eps
        real (kind=kreal) :: sigma
        real (kind=kreal) :: sigma_diag
!
!
        integer (kind=kint) :: itype_mass_matrix
!
        character(len=kchara) :: method_elesize
        character(len=kchara) :: precond_elesize
        integer (kind=kint) :: itr_elesize
        real (kind=kreal) :: eps_elesize
        real (kind=kreal) :: sigma_elesize
        real (kind=kreal) :: sigma_diag_elesize
      end type ctl_params_4_gen_filter
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_moment_parameter(gfil_p)
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!
      allocate(gfil_p%mom_order(gfil_p%num_moments_order))
      allocate(gfil_p%mom_value(gfil_p%num_moments_order))
      allocate(gfil_p%iref_mom_type(gfil_p%num_moments_order))
!
      gfil_p%iref_mom_type = 0
      gfil_p%mom_order = 0
      gfil_p%mom_value = 0.0d0
!
      end subroutine alloc_moment_parameter
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_ref_filter_type(gfil_p)
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!
      allocate(gfil_p%iref_filter_type(gfil_p%num_ref_filter))
      allocate(gfil_p%ref_filter_width(gfil_p%num_ref_filter))
!
      gfil_p%iref_filter_type = iflag_tophat_filter
      gfil_p%ref_filter_width = 0.0d0
!
      end subroutine alloc_ref_filter_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_ref_filter_area(gfil_p)
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!
      allocate(gfil_p%id_filter_area_grp(gfil_p%num_filtering_grp))
      allocate(gfil_p%filter_area_name(gfil_p%num_filtering_grp))
!
      gfil_p%id_filter_area_grp = 0
!
      end subroutine alloc_ref_filter_area
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_moment_parameter(gfil_p)
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!
      deallocate(gfil_p%mom_order)
      deallocate(gfil_p%mom_value)
      deallocate(gfil_p%iref_mom_type)
!
      end subroutine dealloc_moment_parameter
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ref_filter_type(gfil_p)
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!
      deallocate(gfil_p%iref_filter_type)
      deallocate(gfil_p%ref_filter_width)
!
      end subroutine dealloc_ref_filter_type
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_params_4_gen_filter
