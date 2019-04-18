!set_cube_ele_connect.f90
!     module set_cube_ele_connect
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_ele_connect(c_size, c_each, nb_rng, loc_id,      &
!!     &          elm_type, ipe, jpe, ele)
!!      subroutine set_ele_connect_quad(c_size, c_each, nb_rng, loc_id, &
!!     &          elm_type, ipe, jpe, ele)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(size_of_each_cube), intent(in) :: c_each
!!        type(neib_range_cube), intent(in) :: nb_rng
!!        type(local_node_id_cube), intent(in) :: loc_id
!!        type(element_data), intent(inout) :: ele
!
      module set_cube_ele_connect
!
      use m_precision
      use m_geometry_constants
!
      use t_geometry_data
      use t_size_of_cube
      use t_neib_range_cube
      use t_local_node_id_cube
      use set_ele_id_peri
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_ele_connect(c_size, c_each, nb_rng, loc_id,        &
     &          elm_type, ipe, jpe, ele)
!
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(neib_range_cube), intent(in) :: nb_rng
      type(local_node_id_cube), intent(in) :: loc_id
      integer (kind = kint), intent(in) :: elm_type
      integer (kind = kint), intent(in) :: ipe, jpe
!
      type(element_data), intent(inout) :: ele
!
      integer (kind = kint) :: icou
      integer (kind = kint) :: element_id_gl
      integer (kind = kint) :: i, j, k
!
!
       ele%numele = c_each%elmtot
       ele%nnod_4_ele = num_t_linear
       call alloc_element_types(ele)
       call alloc_ele_connectivity(ele)
!
!$omp parallel workshare
      ele%elmtyp(1:ele%numele) = elm_type
      ele%nodelm(1:ele%numele) = ele%nnod_4_ele
!$omp end parallel workshare
!
!
      icou = 0
      do k = 1, c_each%nz-1
       do j = 1, c_each%ny-1
        do i = 1, c_each%nx-1

         call set_element_id_periodic                                   &
     &      (c_size, nb_rng, c_each%nx, c_each%ny,                      &
     &       ipe, jpe, i, j, k, icou, element_id_gl)
!
         ele%iele_global(icou) = element_id_gl
!
         ele%ie(icou, 1) = loc_id%node_id_lc( i  , j  , k   )
         ele%ie(icou, 2) = loc_id%node_id_lc( i+1, j  , k   )
         ele%ie(icou, 3) = loc_id%node_id_lc( i+1, j+1, k   )
         ele%ie(icou, 4) = loc_id%node_id_lc( i  , j+1, k   )
         ele%ie(icou, 5) = loc_id%node_id_lc( i  , j  , k+1 )
         ele%ie(icou, 6) = loc_id%node_id_lc( i+1, j  , k+1 )
         ele%ie(icou, 7) = loc_id%node_id_lc( i+1, j+1, k+1 )
         ele%ie(icou, 8) = loc_id%node_id_lc( i  , j+1, k+1 )
        enddo
       enddo
      enddo
!
      end subroutine set_ele_connect
!
! ----------------------------------------------------------------------
!
      subroutine set_ele_connect_quad(c_size, c_each, nb_rng, loc_id,   &
     &          elm_type, ipe, jpe, ele)
!
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(neib_range_cube), intent(in) :: nb_rng
      type(local_node_id_cube), intent(in) :: loc_id
      integer (kind = kint), intent(in) :: elm_type
      integer (kind = kint), intent(in) :: ipe, jpe
!
      type(element_data), intent(inout) :: ele
!
      integer (kind = kint) :: icou
      integer (kind = kint) :: element_id_gl
      integer (kind = kint) :: i, j, k
!
!
       ele%numele = c_each%elmtot
       ele%nnod_4_ele = num_t_quad
       call alloc_element_types(ele)
       call alloc_ele_connectivity(ele)
!
!$omp parallel workshare
      ele%elmtyp(1:ele%numele) = elm_type
      ele%nodelm(1:ele%numele) = ele%nnod_4_ele
!$omp end parallel workshare
!
      icou = 0
      do k = 1, c_each%nz-1
       do j = 1, c_each%ny-1
        do i = 1, c_each%nx-1
!
         call set_element_id_periodic                                   &
     &      (c_size, nb_rng, c_each%nx, c_each%ny,                      &
     &       ipe, jpe, i, j, k, icou, element_id_gl)
!
         ele%iele_global(icou) = element_id_gl
!
         ele%ie(icou, 1) = loc_id%node_id_lc( i  , j  , k   )
         ele%ie(icou, 2) = loc_id%node_id_lc( i+1, j  , k   )
         ele%ie(icou, 3) = loc_id%node_id_lc( i+1, j+1, k   )
         ele%ie(icou, 4) = loc_id%node_id_lc( i  , j+1, k   )
         ele%ie(icou, 5) = loc_id%node_id_lc( i  , j  , k+1 )
         ele%ie(icou, 6) = loc_id%node_id_lc( i+1, j  , k+1 )
         ele%ie(icou, 7) = loc_id%node_id_lc( i+1, j+1, k+1 )
         ele%ie(icou, 8) = loc_id%node_id_lc( i  , j+1, k+1 )

         ele%ie(icou, 9) = loc_id%edge_id_lc( i  , j  , k  , 1 )
         ele%ie(icou,10) = loc_id%edge_id_lc( i+1, j  , k  , 2 )
         ele%ie(icou,11) = loc_id%edge_id_lc( i  , j+1, k  , 1 )
         ele%ie(icou,12) = loc_id%edge_id_lc( i  , j  , k  , 2 )

         ele%ie(icou,13) = loc_id%edge_id_lc( i  , j  , k+1, 1 )
         ele%ie(icou,14) = loc_id%edge_id_lc( i+1, j  , k+1, 2 )
         ele%ie(icou,15) = loc_id%edge_id_lc( i  , j+1, k+1, 1 )
         ele%ie(icou,16) = loc_id%edge_id_lc( i  , j  , k+1, 2 )

         ele%ie(icou,17) = loc_id%edge_id_lc( i  , j  , k  , 3 )
         ele%ie(icou,18) = loc_id%edge_id_lc( i+1, j  , k  , 3 )
         ele%ie(icou,19) = loc_id%edge_id_lc( i+1, j+1, k  , 3 )
         ele%ie(icou,20) = loc_id%edge_id_lc( i  , j+1, k  , 3 )
        end do
       end do
      end do
!
      end subroutine set_ele_connect_quad
!
! ----------------------------------------------------------------------
!
      end module set_cube_ele_connect
