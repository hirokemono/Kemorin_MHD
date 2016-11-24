!
!      module set_initial_for_MHD
!
!      Written by H. Matsui
!
!!      subroutine set_initial_vect_p(isig, node, ncomp_nod,            &
!!     &          i_vecp, i_magne, i_mag_p, d_nod)
!!      subroutine set_initial_magne(isig, node, ncomp_nod,             &
!!     &          i_magne, i_mag_p, d_nod)
!!      subroutine set_initial_kinematic(numnod, nnod_fl, inod_fluid,   &
!!     &          ncomp_nod, i_velo, i_press, i_magne, d_nod)
!
      module set_initial_for_MHD
!
      use m_precision
      use m_constants
!
      use m_control_parameter
      use m_schmidt_polynomial
!
      use t_geometry_data
      use t_spherical_harmonics
!
      use spherical_harmonics
      use cvt_vector_2_cartecian
!
      implicit none
!
      type(sph_1point_type), private :: sph_ini
!
      real(kind = kreal), private :: v_pole(3), b_pole(3)
      real(kind = kreal), private :: v_cart(3), b_cart(3)
!
      real(kind = kreal), allocatable:: vp(:)
      real(kind = kreal), allocatable:: vt(:)
      real(kind = kreal), allocatable:: dvp(:)
!
      real(kind = kreal), allocatable:: bp(:)
      real(kind = kreal), allocatable:: bt(:)
      real(kind = kreal), allocatable:: dbp(:)
      real(kind = kreal), allocatable:: mp(:)
!
      private :: vp, vt, dvp
      private :: bp, bt, dbp, mp
!
      private :: allocate_initial_bspectr, deallocate_initial_bspectr
      private :: allocate_initial_vspectr, deallocate_initial_vspectr
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_initial_vspectr(jmax)
!
      integer(kind = kint), intent(in) :: jmax
!
!
      allocate ( vp(0:jmax) )
      allocate ( vt(0:jmax) )
      allocate ( dvp(0:jmax) )
!
      vp = 0.0d0
      vt = 0.0d0
      dvp = 0.0d0
!
      end subroutine allocate_initial_vspectr
!
! -----------------------------------------------------------------------
!
      subroutine allocate_initial_bspectr(jmax)
!
      integer(kind = kint), intent(in) :: jmax
!
!
       allocate ( bp(0:jmax) )
       allocate ( bt(0:jmax) )
       allocate ( dbp(0:jmax) )
       allocate ( mp(0:jmax) )
!
        bp = 0.0d0
        bt = 0.0d0
        dbp = 0.0d0
        mp = 0.0d0
!
       end subroutine allocate_initial_bspectr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_initial_vspectr
!
      deallocate(vp, vt, dvp)
!
      end subroutine deallocate_initial_vspectr
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_initial_bspectr
!
!
      deallocate(bp, bt, dbp, mp)
!
       end subroutine deallocate_initial_bspectr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_initial_vect_p(isig, node, ncomp_nod,              &
     &          i_vecp, i_magne, i_mag_p, d_nod)
!
      use m_physical_property
      use spherical_harmonics
      use sph_spectr_2_vector
!
      use dynamobench_r_func_sph_vecp
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: isig
      integer(kind = kint), intent(in) :: ncomp_nod
      integer(kind = kint), intent(in) :: i_vecp, i_magne, i_mag_p
      real(kind = kreal), intent(inout) :: d_nod(node%numnod,ncomp_nod)
!
      integer(kind = kint) :: inod, ifl, j_rst, l_rst, m_rst
!
!
      j_rst = ( isig-mod(isig,ikilo) )/ikilo
      call get_dgree_order_by_full_j(j_rst, l_rst, m_rst)
!
      ifl = 1
      if ( abs(depth_low_t/depth_high_t - 0.35) .lt. 1.0d-4) ifl = 2
!
      call allocate_schmidt_polynomial
      call init_sph_indices(nth, sph_ini)
      call alloc_spherical_harmonics(sph_ini)
      call allocate_initial_bspectr(sph_ini%jmax_tri)
!
        do inod = 1, node%numnod
         call dschmidt(node%theta(inod))
         call spheric                                                   &
     &     (sph_ini%jmax_tri, sph_ini%idx, node%phi(inod), sph_ini%y_lm)
!
         call radial_function_sph_vecp                                  &
     &      (sph_ini%jmax_tri, ifl, j_rst, l_rst, node%rr(inod),        &
     &       depth_high_t, depth_low_t, bp, bt, dbp, mp)
!
!         d_nod(inod,i_mag_p) = 0.0d0
!         do j = 1, sph_ini%jmax_tri
!          d_nod(inod,i_mag_p)                                          &
!     &            = d_nod(inod,i_mag_p) + mp(j)*sph_ini%y_lm(j,0)
!         end do
!
         call cvt_spectr_2_vector(sph_ini%ltr_tri, sph_ini%jmax_tri,    &
     &       node%rr(inod), node%theta(inod), sph_ini%g, sph_ini%y_lm,  &
     &       bp, bt, dbp, b_pole)
!
         call cvt_one_vector_2_cart                                     &
     &      (b_cart, b_pole, node%theta(inod), node%phi(inod))
         d_nod(inod,i_vecp:i_vecp+2) = b_cart(1:3)
!
         if ( node%rr(inod) .le. 1.0d-20 ) then
           d_nod(inod,i_magne:i_magne+2) = 0.0d0
           d_nod(inod,i_mag_p) =           0.0d0
         end if
      end do
!
      call deallocate_initial_bspectr
      call deallocate_schmidt_polynomial
      call dealloc_spherical_harmonics(sph_ini)
      call dealloc_index_4_sph(sph_ini)
!
      end subroutine set_initial_vect_p
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_magne(isig, node, ncomp_nod,               &
     &          i_magne, i_mag_p, d_nod)
!
      use m_physical_property
!
      use radial_func_sph_magne
      use spherical_harmonics
      use sph_spectr_2_vector
!
      type(node_data), intent(in) :: node
      integer ( kind = kint), intent(in) :: isig
      integer(kind = kint), intent(in) :: ncomp_nod, i_magne, i_mag_p
      real(kind = kreal), intent(inout) :: d_nod(node%numnod,ncomp_nod)
!
      integer ( kind = kint) :: inod, j, ifl, j_rst, l_rst, m_rst
!
!
      j_rst = ( isig-mod(isig,ikilo) )/ikilo
      call get_dgree_order_by_full_j(j_rst, l_rst, m_rst)
!
      ifl = 1
      if ( abs(depth_low_t/depth_high_t - 0.35) .lt. 1.0d-4) ifl = 2
!
      call allocate_schmidt_polynomial
      call init_sph_indices(nth, sph_ini)
      call alloc_spherical_harmonics(sph_ini)
      call allocate_initial_bspectr(sph_ini%jmax_tri)
!
        do inod = 1, node%numnod
         call dschmidt(node%theta(inod))
         call spheric                                                   &
     &     (sph_ini%jmax_tri, sph_ini%idx, node%phi(inod), sph_ini%y_lm)
!
         call radial_function_sph                                       &
     &      (sph_ini%jmax_tri, ifl, j_rst, l_rst, node%rr(inod),        &
     &       depth_high_t, depth_low_t, bp, bt, dbp, mp)
!
         d_nod(inod,i_mag_p) = 0.0d0
         do j = 1, sph_ini%jmax_tri
          d_nod(inod,i_mag_p)                                           &
     &             = d_nod(inod,i_mag_p) + mp(j)*sph_ini%y_lm(j,0)
         end do
!
         call cvt_spectr_2_vector(sph_ini%ltr_tri, sph_ini%jmax_tri,    &
     &       node%rr(inod), node%theta(inod), sph_ini%g, sph_ini%y_lm,  &
     &       bp, bt, dbp, b_pole)
!
         call cvt_one_vector_2_cart                                     &
     &      (b_cart, b_pole, node%theta(inod), node%phi(inod))
         d_nod(inod,i_magne:i_magne+2) = b_cart(1:3)
!
         if ( node%rr(inod)  .le. 1.0d-20 ) then
           d_nod(inod,i_magne  ) = 0.0d0
           d_nod(inod,i_magne+1) = 0.0d0
           d_nod(inod,i_magne+2) = 2.83351986832173d-1
           d_nod(inod,i_mag_p)   = 0.0d0
         end if
!
      end do
!
      call deallocate_initial_bspectr
      call deallocate_schmidt_polynomial
      call dealloc_spherical_harmonics(sph_ini)
      call dealloc_index_4_sph(sph_ini)
!
      end subroutine set_initial_magne
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_kinematic(node, nnod_fl, inod_fluid,       &
     &          ncomp_nod, i_velo, i_press, i_magne, d_nod)
!
      use dynamobench_r_func_sph_velo
      use sph_spectr_2_vector
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: nnod_fl, ncomp_nod
      integer(kind = kint), intent(in) :: inod_fluid(nnod_fl)
      integer(kind = kint), intent(in) :: i_velo, i_press, i_magne
      real(kind = kreal), intent(inout) :: d_nod(node%numnod,ncomp_nod)
!
      integer (kind = kint) :: inod, inum
!
!
      do inod = 1, node%numnod
       d_nod(inod,i_press) = 0.0d0
      end do
!
      call allocate_schmidt_polynomial
      call init_sph_indices(nth, sph_ini)
      call alloc_spherical_harmonics(sph_ini)
      call allocate_initial_bspectr(sph_ini%jmax_tri)
      call allocate_initial_vspectr(sph_ini%jmax_tri)
!
        do inum = 1, nnod_fl
         inod = inod_fluid(inum)
!
         call dschmidt(node%theta(inod))
         call spheric                                                   &
     &     (sph_ini%jmax_tri, sph_ini%idx, node%phi(inod), sph_ini%y_lm)
!
         call radial_function_sph_velo(sph_ini%jmax_tri, node%rr(inod), &
     &       vp, vt, dvp)
!
         call cvt_spectr_2_vector(sph_ini%ltr_tri, sph_ini%jmax_tri,    &
     &       node%rr(inod), node%theta(inod), sph_ini%g, sph_ini%y_lm,  &
     &       vp, vt, dvp, v_pole)
         call cvt_spectr_2_vector(sph_ini%ltr_tri, sph_ini%jmax_tri,    &
     &       node%rr(inod), node%theta(inod), sph_ini%g, sph_ini%y_lm,  &
     &       bp, bt, dbp, b_pole)
!
         call cvt_one_vector_2_cart                                     &
     &      (v_cart, v_pole, node%theta(inod), node%phi(inod))
         call cvt_one_vector_2_cart                                     &
     &      (b_cart, b_pole, node%theta(inod), node%phi(inod))
!
         d_nod(inod,i_velo:i_velo+2) =   v_cart(1:3)
         d_nod(inod,i_magne:i_magne+2) = b_cart(1:3)
      end do
!
      call deallocate_initial_vspectr
      call deallocate_initial_bspectr
      call deallocate_schmidt_polynomial
      call dealloc_spherical_harmonics(sph_ini)
      call dealloc_index_4_sph(sph_ini)
!
      end subroutine set_initial_kinematic
!
!-----------------------------------------------------------------------
!
      end module set_initial_for_MHD
