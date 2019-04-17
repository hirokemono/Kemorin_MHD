!set_cube_ele_connect.f90
!     module set_cube_ele_connect
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine set_ele_connect                                      &
!!     &         (c_size, c_each, nb_rng, loc_id, elm_type, ipe, jpe)
!!      subroutine set_ele_connect_quad                                 &
!!     &         (c_size, c_each, nb_rng, loc_id, elm_type, ipe, jpe)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(size_of_each_cube), intent(in) :: c_each
!!        type(neib_range_cube), intent(in) :: nb_rng
!!        type(local_node_id_cube), intent(in) :: loc_id
!
      module set_cube_ele_connect
!
      use m_precision
!
      use t_size_of_cube
      use t_neib_range_cube
      use t_local_node_id_cube
      use m_cube_files_data
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
      subroutine set_ele_connect                                        &
     &         (c_size, c_each, nb_rng, loc_id, elm_type, ipe, jpe)
!
      use m_fem_mesh_labels
!
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(neib_range_cube), intent(in) :: nb_rng
      type(local_node_id_cube), intent(in) :: loc_id
      integer (kind = kint), intent(in) :: elm_type
      integer (kind = kint), intent(in) :: ipe, jpe
!
      integer (kind = kint) :: element_id
      integer (kind = kint) :: element_id_gl
      integer (kind = kint) :: i, j, k
      integer (kind = kint) :: i1, i2, i3, i4, i5, i6, i7, i8
!
!
! ..... write 2.2 element (connection)
!
      write(l_out,'(a)', advance='NO') hd_fem_elem()
      write(l_out,'(10i16)')   c_each%elmtot
      write(l_out,'(10i16)')  (elm_type,i=1,c_each%elmtot)

      element_id = 0

      do k = 1, c_each%nz-1
       do j = 1, c_each%ny-1
        do i = 1, c_each%nx-1

         call set_element_id_periodic                                   &
     &      (c_size, nb_rng, c_each%nx, c_each%ny,                      &
     &       ipe, jpe, i, j, k, element_id, element_id_gl)
!
         i1 = loc_id%node_id_lc( i  , j  , k   )
         i2 = loc_id%node_id_lc( i+1, j  , k   )
         i3 = loc_id%node_id_lc( i+1, j+1, k   )
         i4 = loc_id%node_id_lc( i  , j+1, k   )
         i5 = loc_id%node_id_lc( i  , j  , k+1 )
         i6 = loc_id%node_id_lc( i+1, j  , k+1 )
         i7 = loc_id%node_id_lc( i+1, j+1, k+1 )
         i8 = loc_id%node_id_lc( i  , j+1, k+1 )

         write(l_out,'(9i16)')  element_id_gl,                          &
     &                          i1, i2, i3, i4, i5, i6, i7, i8
!
        enddo
       enddo
      enddo
!
      end subroutine set_ele_connect
!
! ----------------------------------------------------------------------
!
      subroutine set_ele_connect_quad                                   &
     &         (c_size, c_each, nb_rng, loc_id, elm_type, ipe, jpe)
!
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(neib_range_cube), intent(in) :: nb_rng
      type(local_node_id_cube), intent(in) :: loc_id
      integer (kind = kint), intent(in) :: elm_type
      integer (kind = kint), intent(in) :: ipe, jpe
!
      integer (kind = kint) :: element_id
      integer (kind = kint) :: element_id_gl
      integer (kind = kint) :: i, j, k
      integer (kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
      integer (kind = kint) :: i9,  i10, i11, i12, i13, i14, i15, i16
      integer (kind = kint) :: i17, i18, i19, i20
!
!
! ..... write 2.2 element (connection)
!
      write(l_out,'( a )') '! 2.2 element (connection)'
      write(l_out,'(10i16)')   c_each%elmtot
      write(l_out,'(10i16)')  (elm_type,i=1,c_each%elmtot)

      element_id = 0

      do k = 1, c_each%nz-1
       do j = 1, c_each%ny-1
        do i = 1, c_each%nx-1
!
         call set_element_id_periodic                                   &
     &      (c_size, nb_rng, c_each%nx, c_each%ny,                      &
     &       ipe, jpe, i, j, k, element_id, element_id_gl)
!
!
         i1  = loc_id%node_id_lc( i  , j  , k   )
         i2  = loc_id%node_id_lc( i+1, j  , k   )
         i3  = loc_id%node_id_lc( i+1, j+1, k   )
         i4  = loc_id%node_id_lc( i  , j+1, k   )
         i5  = loc_id%node_id_lc( i  , j  , k+1 )
         i6  = loc_id%node_id_lc( i+1, j  , k+1 )
         i7  = loc_id%node_id_lc( i+1, j+1, k+1 )
         i8  = loc_id%node_id_lc( i  , j+1, k+1 )

         i9  = loc_id%edge_id_lc( i  , j  , k  , 1 )
         i10 = loc_id%edge_id_lc( i+1, j  , k  , 2 )
         i11 = loc_id%edge_id_lc( i  , j+1, k  , 1 )
         i12 = loc_id%edge_id_lc( i  , j  , k  , 2 )

         i13 = loc_id%edge_id_lc( i  , j  , k+1, 1 )
         i14 = loc_id%edge_id_lc( i+1, j  , k+1, 2 )
         i15 = loc_id%edge_id_lc( i  , j+1, k+1, 1 )
         i16 = loc_id%edge_id_lc( i  , j  , k+1, 2 )

         i17 = loc_id%edge_id_lc( i  , j  , k  , 3 )
         i18 = loc_id%edge_id_lc( i+1, j  , k  , 3 )
         i19 = loc_id%edge_id_lc( i+1, j+1, k  , 3 )
         i20 = loc_id%edge_id_lc( i  , j+1, k  , 3 )

         write(l_out,'(21i16)')  element_id_gl,                         &
     &               i1 , i2 , i3 , i4 , i5 , i6 , i7 , i8 , i9 , i10,  &
     &               i11, i12, i13, i14, i15, i16, i17, i18, i19, i20
        end do
       end do
      end do
!
      end subroutine set_ele_connect_quad
!
! ----------------------------------------------------------------------
!
      end module set_cube_ele_connect
      
