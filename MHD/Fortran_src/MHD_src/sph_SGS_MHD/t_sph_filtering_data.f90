!>@file   t_sph_filtering_data.f90
!!@brief  module t_sph_filtering_data
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Evaluate horizontal filtering in spectrunm space
!!
!!@verbatim
!!        type(sph_filter_moment), intent(inout) :: mom
!!      subroutine alloc_sph_2nd_filter_moments(sph_rtp, sph_filters)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_filters_type), intent(inout) :: sph_filters
!!
!!      subroutine init_sph_2nd_order_moments_rtp                       &
!!     &         (sph_rtp, sph_rj, radial_rtp_grp, leg, sph_filters)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(group_data), intent(in) :: radial_rtp_grp
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(sph_filters_type), intent(inout) :: sph_filters
!!
!!      subroutine dealloc_sph_filter_weights(sph_filter)
!!        type(sph_gaussian_filter), intent(inout) :: sph_filter
!!      subroutine dealloc_sph_2nd_filter_moments(sph_filters)
!!        type(sph_filters_type), intent(inout) :: sph_filters
!!
!!      subroutine check_radial_filter(sph_rj, r_filter)
!!      subroutine check_radial_filter_func(sph_rj, r_filter)
!!      subroutine check_sph_2nd_moments(sph_rtp, sph_filters)
!!@endverbatim
!!
!
      module t_sph_filtering_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_sph_filter_moment
      use t_filter_coefficients
      use t_spheric_parameter
!
      implicit none
!
!
      character(len=kchara), parameter :: gaussian_label =  'gaussian'
      character(len=kchara), parameter :: cutoff_label =    'cutoff'
      character(len=kchara), parameter :: recursive_label = 'recursive'
      integer(kind = kint), parameter :: iflag_gaussian_filter =   0
      integer(kind = kint), parameter :: iflag_cutoff_filter =    10
      integer(kind = kint), parameter :: iflag_recursive_filter = 20
!
!
      type sph_gaussian_filter
!>        Truncation degree
        integer(kind = kint) :: l_truncation
!>        filter width
        real(kind = kreal) :: f_width
!>        Coefficients for each degree
        real(kind = kreal), allocatable :: weight(:)
      end type sph_gaussian_filter
!
!
!>      Structure for filtering data for spherical shell
      type sph_filters_type
        real(kind = kreal) :: width = 1.0d0
!>        integer flag of filter function on sphere
        integer(kind = kint)                                            &
     &        :: itype_sph_filter =    iflag_gaussian_filter
!>        integer flag of radial filter function
        integer(kind = kint)                                            &
     &        :: itype_radial_filter = iflag_gaussian_filter
!
!>        1st reference filter ID for multiplied filter
        integer(kind = kint) :: id_1st_ref_filter = ione
!>        2nd reference filter ID for multiplied filter
        integer(kind = kint) :: id_2nd_ref_filter = ione
!
!>        data structure for radial filter coefficients table
        type(filter_coefficients_type) :: r_filter
!>        data structure for horizontral filter coefficients table
        type(sph_gaussian_filter) :: sph_filter
!
!>        data structure for radial filter moments table
        type(sph_filter_moment) :: r_moments
!>        data structure for horizontral filter moments table
        type(sph_filter_moment) :: sph_moments
!
!>        Start of fluid area (local radial level in 9r, theta, phi)
        integer(kind = kint) :: kr_SGS_in
!>        End of fluid area (local radial level in 9r, theta, phi)
        integer(kind = kint) :: kr_SGS_out
!
!>        second filter moments in radial direction
        real(kind = kreal), allocatable :: radial_2nd_moment(:)
!>        second filter moments in theta direction
        real(kind = kreal), allocatable :: theta_2nd_moment(:)
!>        second filter moments in phi direction
        real(kind = kreal) :: phi_2nd_moment
      end type sph_filters_type
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_sph_filter_weights(ltr, sph_filter)
!
      integer(kind = kint), intent(in) :: ltr
      type(sph_gaussian_filter), intent(inout) :: sph_filter
!
!
      sph_filter%l_truncation = ltr
      allocate(sph_filter%weight(0:sph_filter%l_truncation))
      sph_filter%weight = 1.0d0
!
      end subroutine alloc_sph_filter_weights
!
! ----------------------------------------------------------------------
!
      subroutine alloc_sph_2nd_filter_moments(sph_rtp, sph_filters)
!
      use t_spheric_rtp_data
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_filters_type), intent(inout) :: sph_filters
!
      integer(kind = kint) :: nri, nth
!
!
      nri = sph_rtp%nidx_rtp(1)
      nth = sph_rtp%nidx_rtp(2)
      allocate(sph_filters%radial_2nd_moment(nri))
      allocate(sph_filters%theta_2nd_moment(nth))
      if(nri .gt. 0) sph_filters%radial_2nd_moment = 0.0d0
      if(nth .gt. 0) sph_filters%theta_2nd_moment =  0.0d0
      sph_filters%phi_2nd_moment = 0.0d0
!
      end subroutine alloc_sph_2nd_filter_moments
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_sph_2nd_order_moments_rtp                         &
     &         (sph_rtp, sph_rj, radial_rtp_grp, leg, sph_filters)
!
      use t_spheric_rtp_data
      use t_spheric_rj_data
      use t_schmidt_poly_on_rtm
      use t_group_data
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(group_data), intent(in) :: radial_rtp_grp
      type(legendre_4_sph_trans), intent(in) :: leg
!
      type(sph_filters_type), intent(inout) :: sph_filters
!
!
      call alloc_sph_2nd_filter_moments(sph_rtp, sph_filters)
      call cal_sph_2nd_order_moments_rtp(sph_rtp, sph_rj, leg,          &
     &    sph_filters%r_moments, sph_filters%sph_moments,               &
     &    sph_filters%radial_2nd_moment, sph_filters%theta_2nd_moment,  &
     &    sph_filters%phi_2nd_moment)
!
      end subroutine init_sph_2nd_order_moments_rtp
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_sph_filter_weights(sph_filter)
!
      type(sph_gaussian_filter), intent(inout) :: sph_filter
!
!
      deallocate(sph_filter%weight)
!
      end subroutine dealloc_sph_filter_weights
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_sph_2nd_filter_moments(sph_filters)
!
      type(sph_filters_type), intent(inout) :: sph_filters
!
!
      deallocate(sph_filters%radial_2nd_moment)
      deallocate(sph_filters%theta_2nd_moment)
!
      end subroutine dealloc_sph_2nd_filter_moments
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_radial_filter(sph_rj, r_filter)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(filter_coefficients_type), intent(inout) :: r_filter
!
      integer(kind = kint) :: i, ist, ied
!
!
      if(my_rank .ne. 0) return
        write(*,*)  'r_filter%inod_filter(i)',  r_filter%istack_node
        do i = r_filter%istack_node(0)+1, r_filter%istack_node(1)
          ist = r_filter%istack_near_nod(i-1) + 1
          ied = r_filter%istack_near_nod(i)
          write(*,*) i, r_filter%inod_filter(i),                        &
     &                  r_filter%inod_near(ist:ied)
        end do
        write(*,*)  'r_filter%weight(i)'
        do i = r_filter%istack_node(0)+1, r_filter%istack_node(1)
          ist = r_filter%istack_near_nod(i-1) + 1
          ied = r_filter%istack_near_nod(i)
          write(*,*) sph_rj%radius_1d_rj_r(r_filter%inod_filter(i)),    &
     &               i, r_filter%inod_filter(i),                        &
     &                  r_filter%weight(ist:ied)
        end do
!
      end subroutine check_radial_filter
!
! ----------------------------------------------------------------------
!
      subroutine check_radial_filter_func(sph_rj, r_filter)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(filter_coefficients_type), intent(inout) :: r_filter
!
      integer(kind = kint) :: i, ist, ied
!
!
      if(my_rank .ne. 0) return
        write(*,*)  'r_filter%func(i)'
        do i = r_filter%istack_node(0)+1, r_filter%istack_node(1)
          ist = r_filter%istack_near_nod(i-1) + 1
          ied = r_filter%istack_near_nod(i)
          write(*,*) sph_rj%radius_1d_rj_r(r_filter%inod_filter(i)),    &
     &               i, r_filter%inod_filter(i),                        &
     &                  r_filter%func(ist:ied)
        end do
!
      end subroutine check_radial_filter_func
!
! ----------------------------------------------------------------------
!
      subroutine check_horiz_filter_weight(sph_filter)
!
      type(sph_gaussian_filter), intent(in) :: sph_filter
!
      integer(kind = kint) :: l
!
!
      write(*,*)  'horizontal_filter', sph_filter%f_width
      do l = 0, sph_filter%l_truncation
        write(*,*) l, sph_filter%weight(l)
      end do
!
      end subroutine check_horiz_filter_weight
!
! ----------------------------------------------------------------------
!
      subroutine check_sph_2nd_moments(sph_rtp, sph_filters)
!
      use t_spheric_rtp_data
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_filters_type), intent(in) :: sph_filters
!
      integer(kind = kint) :: k, l
!
!
      write(*,*) 'Second order filter moments area: ',                  &
     &          sph_filters%kr_SGS_in, sph_filters%kr_SGS_out
      write(*,*) 'Radial-direction, global ID, radius, moments'
      do k = 1, sph_rtp%nidx_rtp(1)
        write(*,*) k, sph_rtp%idx_gl_1d_rtp_r(k),                       &
     &    sph_rtp%radius_1d_rtp_r(k), sph_filters%radial_2nd_moment(k)
      end do
      write(*,*) 'Theta-direction, global ID, moments'
      do l = 1, sph_rtp%nidx_rtp(2)
        write(*,*) l, sph_rtp%idx_gl_1d_rtp_t(l),                       &
     &             sph_filters%theta_2nd_moment(l)
      end do
      write(*,*) 'Phi-direction', sph_filters%phi_2nd_moment
!
      end subroutine check_sph_2nd_moments
!
! ----------------------------------------------------------------------
!
      end module t_sph_filtering_data
