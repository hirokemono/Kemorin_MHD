!
!      module m_ctl_params_4_gen_filter
!
!     Written by H. Matsui on July, 2006
!
      module m_ctl_params_4_gen_filter
!
      use m_precision
!
      implicit none
!
!
      character(len = kchara) :: org_filter_3d_head
      character(len = kchara) :: org_filter_coef_head
      character(len = kchara) :: org_filter_elen_head
      character(len = kchara) :: org_filter_moms_head
!
      integer(kind = kint) :: num_int_points
      integer(kind = kint) :: minimum_comp
      integer(kind = kint) :: maximum_neighbour = 1
      integer(kind = kint) :: ist_num_free = -1, ied_num_free = -1
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
!
      integer (kind=kint) :: id_solver_type
!
      character(len=kchara) :: method
      character(len=kchara) :: precond
      integer (kind=kint) :: itr
      real (kind=kreal) :: eps
      real (kind=kreal) :: sigma
      real (kind=kreal) :: sigma_diag
!
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
!
!
!
      integer (kind=kint) :: INTARRAY(2)
      real (kind=kreal) ::   REALARRAY(3)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_moment_parameter
!
      allocate(mom_order(num_moments_order))
      allocate(mom_value(num_moments_order))
      allocate(iref_mom_type(num_moments_order))
!
      iref_mom_type = 0
      mom_order = 0
      mom_value = 0.0d0
!
      end subroutine allocate_moment_parameter
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ref_filter_type
!
      allocate(iref_filter_type(num_ref_filter))
      allocate(ref_filter_width(num_ref_filter))
!
      iref_filter_type = 0
      ref_filter_width = 0.0d0
!
      end subroutine allocate_ref_filter_type
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ref_filter_area
!
      allocate(id_filter_area_grp(num_filtering_grp))
      allocate(filter_area_name(num_filtering_grp))
!
      id_filter_area_grp = 0
!
      end subroutine allocate_ref_filter_area
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_moment_parameter
!
      deallocate(mom_order)
      deallocate(mom_value)
      deallocate(iref_mom_type)
!
      end subroutine deallocate_moment_parameter
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ref_filter_type
!
      deallocate(iref_filter_type)
      deallocate(ref_filter_width)
!
      end subroutine deallocate_ref_filter_type
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_params_4_gen_filter
