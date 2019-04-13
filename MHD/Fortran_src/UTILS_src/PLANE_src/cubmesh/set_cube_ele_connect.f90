!set_cube_ele_connect.f90
!     module set_cube_ele_connect
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!     subroutine set_ele_connect(nb_rng, elm_type, ipe, jpe, kpe)
!!     subroutine set_ele_connect_quad(nb_rng, elm_type, ipe, jpe, kpe)
!!       type(neib_range_cube), intent(in) :: nb_rng
!
      module set_cube_ele_connect
!
      use m_precision
!
      use t_neib_range_cube
      use m_size_of_cube
      use m_local_node_id_cube
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
      subroutine set_ele_connect(nb_rng, elm_type, ipe, jpe, kpe)
!
      use m_fem_mesh_labels
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: elm_type
      integer (kind = kint), intent(in) :: ipe, jpe, kpe
!
      integer (kind = kint) :: element_id
      integer (kind = kint) :: element_id_gl
      integer (kind = kint) :: i, j, k
      integer (kind = kint) :: i1, i2, i3, i4, i5, i6, i7, i8
!
!
      elmtot =       (nx-1)*(ny-1)*(nz-1)
      elm_fil1_tot = (nx1-1)*(ny1-1)*(nz1-1)
!
! ..... write 2.2 element (connection)
!
      write(l_out,'(a)', advance='NO') hd_fem_elem()
      write(l_out,'(10i16)')   elmtot
      write(l_out,'(10i16)')  (elm_type,i=1,elmtot)

      element_id = 0

      do k=1,nz-1
       do j=1,ny-1
        do i=1,nx-1

         call set_element_id_periodic(nb_rng, ipe, jpe, kpe, i, j, k,   &
     &       element_id, element_id_gl)
!
         i1 = node_id_lc( i  , j  , k   )
         i2 = node_id_lc( i+1, j  , k   )
         i3 = node_id_lc( i+1, j+1, k   )
         i4 = node_id_lc( i  , j+1, k   )
         i5 = node_id_lc( i  , j  , k+1 )
         i6 = node_id_lc( i+1, j  , k+1 )
         i7 = node_id_lc( i+1, j+1, k+1 )
         i8 = node_id_lc( i  , j+1, k+1 )

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
      subroutine set_ele_connect_quad(nb_rng, elm_type, ipe, jpe, kpe)
!
      type(neib_range_cube), intent(in) :: nb_rng
      integer (kind = kint), intent(in) :: elm_type
      integer (kind = kint), intent(in) :: ipe, jpe, kpe
!
      integer (kind = kint) :: element_id
      integer (kind = kint) :: element_id_gl
      integer (kind = kint) :: i, j, k
      integer (kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
      integer (kind = kint) :: i9,  i10, i11, i12, i13, i14, i15, i16
      integer (kind = kint) :: i17, i18, i19, i20
!
!
      elmtot =       (nx-1)*(ny-1)*(nz-1)
      elm_fil1_tot = (nx1-1)*(ny1-1)*(nz1-1)
!
! ..... write 2.2 element (connection)
!
      write(l_out,'( a )') '! 2.2 element (connection)'
      write(l_out,'(10i16)')   elmtot
      write(l_out,'(10i16)')  (elm_type,i=1,elmtot)

      element_id = 0

      do k=1,nz-1
       do j=1,ny-1
        do i=1,nx-1
!
         call set_element_id_periodic(nb_rng, ipe, jpe, kpe, i, j, k,   &
     &       element_id, element_id_gl)
!
!
         i1  = node_id_lc( i  , j  , k   )
         i2  = node_id_lc( i+1, j  , k   )
         i3  = node_id_lc( i+1, j+1, k   )
         i4  = node_id_lc( i  , j+1, k   )
         i5  = node_id_lc( i  , j  , k+1 )
         i6  = node_id_lc( i+1, j  , k+1 )
         i7  = node_id_lc( i+1, j+1, k+1 )
         i8  = node_id_lc( i  , j+1, k+1 )

         i9  = edge_id_lc( i  , j  , k  , 1 )
         i10 = edge_id_lc( i+1, j  , k  , 2 )
         i11 = edge_id_lc( i  , j+1, k  , 1 )
         i12 = edge_id_lc( i  , j  , k  , 2 )

         i13 = edge_id_lc( i  , j  , k+1, 1 )
         i14 = edge_id_lc( i+1, j  , k+1, 2 )
         i15 = edge_id_lc( i  , j+1, k+1, 1 )
         i16 = edge_id_lc( i  , j  , k+1, 2 )

         i17 = edge_id_lc( i  , j  , k  , 3 )
         i18 = edge_id_lc( i+1, j  , k  , 3 )
         i19 = edge_id_lc( i+1, j+1, k  , 3 )
         i20 = edge_id_lc( i  , j+1, k  , 3 )

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
      
