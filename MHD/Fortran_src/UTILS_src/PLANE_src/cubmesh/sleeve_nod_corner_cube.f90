!sleeve_nod_corner_cube.f90
!      module sleeve_nod_corner_cube
!
!!      subroutine set_sleeve_node_xmin_ymin                            &
!!     &         (c_size, c_vert, sl_rng, ioff_gl, koff, inod)
!!      subroutine set_sleeve_node_xmax_ymin                            &
!!     &         (c_size, c_vert, sl_rng, ioff_gl, koff, inod)
!!      subroutine set_sleeve_node_xmax_ymax                            &
!!     &         (c_size, c_vert, sl_rng, ioff_gl, koff, inod)
!!      subroutine set_sleeve_node_xmin_ymax                            &
!!     &         (c_size, c_vert, sl_rng, ioff_gl, koff, inod)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(slleve_range), intent(in) :: sl_rng
!
      module sleeve_nod_corner_cube
!
      use m_precision
!
      use t_size_of_cube
      use t_sleeve_cube
      use m_local_node_id_cube
      use t_cube_position
      use m_cube_files_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_node_xmin_ymin                              &
     &         (c_size, c_vert, sl_rng, ioff_gl, koff, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl, koff
      integer (kind = kint), intent(inout) :: inod
!
      integer (kind = kint) :: node_id_gl
      integer (kind = kint) :: i, j, k
      real (kind = kreal) :: x, y, z
!
!
      do k = sl_rng%ks, sl_rng%ke
       do j = 1, c_size%ndepth
        do i = 1, c_size%ndepth

         inod = inod + 1

         node_id_lc(i,j,k) =  inod
         node_id_gl        = ioff_gl + i + (j-1) * c_size%ndepth        &
     &                      + (koff+k-1)*c_size%ndepth**2

         x = c_size%xmin + (i-1) * c_size%xsize / dble(c_size%nx_all)
         y = c_size%ymin + (j-1) * c_size%ysize / dble(c_size%ny_all)
         z = c_vert%zz(koff+k)

         write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z

        end do
       end do
      end do
!
      end subroutine set_sleeve_node_xmin_ymin
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_node_xmax_ymin                              &
     &         (c_size, c_vert, sl_rng, ioff_gl, koff, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl, koff
      integer (kind = kint), intent(inout) :: inod

!
      integer (kind = kint) :: node_id_gl
      integer (kind = kint) :: i, j, k, i1
      real (kind = kreal) :: x, y, z
!
!
      do k = sl_rng%ks, sl_rng%ke
       do j = 1, c_size%ndepth
        do i = 1, c_size%ndepth

          inod = inod + 1
          i1 = c_size%nxi + c_size%ndepth + i

         node_id_lc(i1,j,k) =  inod
         node_id_gl        = ioff_gl + i + (j-1) * c_size%ndepth        &
     &                      + (koff+k-1)*c_size%ndepth**2

         x = c_size%xmax + (i+c_size%ndepth-1)                          &
     &                    * c_size%xsize / dble(c_size%nx_all)
         y = c_size%ymin + (j-1) * c_size%ysize / dble(c_size%ny_all)
         z = c_vert%zz(koff+k)

          write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z

        end do
       end do
      end do
!
      end subroutine set_sleeve_node_xmax_ymin
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_node_xmax_ymax                              &
     &         (c_size, c_vert, sl_rng, ioff_gl, koff, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl, koff
      integer (kind = kint), intent(inout) :: inod

!
      integer (kind= kint) :: node_id_gl
      integer (kind= kint) :: i, j, k, i1, j1
      real (kind= kreal) :: x, y, z
!
!
      do k = sl_rng%ks, sl_rng%ke
       do j = 1, c_size%ndepth
        do i = 1, c_size%ndepth

          inod = inod + 1
          i1 = c_size%nxi + c_size%ndepth + i
          j1 = c_size%nyi + c_size%ndepth + j

         node_id_lc(i1,j1,k) = inod
         node_id_gl        = ioff_gl + i + (j-1) * c_size%ndepth        &
     &                      + (koff+k-1)*c_size%ndepth**2

         x = c_size%xmax + (i+c_size%ndepth-1)                          &
     &                    * c_size%xsize / dble(c_size%nx_all)
         y = c_size%ymax + (j+c_size%ndepth-1)                          &
     &                    * c_size%ysize / dble(c_size%ny_all)
         z = c_vert%zz(koff+k)

         write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z

        end do
       end do
      end do
!
      end subroutine set_sleeve_node_xmax_ymax
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_node_xmin_ymax                              &
     &         (c_size, c_vert, sl_rng, ioff_gl, koff, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(vertical_position_cube), intent(in) :: c_vert
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl, koff
      integer (kind = kint), intent(inout) :: inod

!
      integer (kind= kint) :: node_id_gl
      integer (kind= kint) :: i, j, k, j1
      real (kind= kreal) :: x, y, z
!
!
      do k = sl_rng%ks, sl_rng%ke
       do j = 1, c_size%ndepth
        do i = 1, c_size%ndepth

         inod = inod + 1
          j1 = c_size%nyi + c_size%ndepth + j

         node_id_lc(i,j1,k) = inod
         node_id_gl        = ioff_gl + i + (j-1) * c_size%ndepth        &
     &                      + (koff+k-1)*c_size%ndepth**2

         x = c_size%xmin + (i-1) * c_size%xsize / dble(c_size%nx_all)
         y = c_size%ymax + (j+c_size%ndepth-1)                          &
     &                    * c_size%ysize / dble(c_size%ny_all)
         z = c_vert%zz(koff+k)

         write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z

        end do
       end do
      end do
!
      end subroutine set_sleeve_node_xmin_ymax
!
!  ---------------------------------------------------------------------
!
      end module sleeve_nod_corner_cube
