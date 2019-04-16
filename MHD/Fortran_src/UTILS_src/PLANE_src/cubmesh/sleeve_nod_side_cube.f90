!
!     module sleeve_nod_side_cube
!
!      Written by Kemorin
!
!!      subroutine set_sleeve_node_xmin                                 &
!!     &         (c_size, nb_rng, sl_rng, ioff_gl, inod)
!!      subroutine set_sleeve_node_xmax                                 &
!!     &         (c_size, nb_rng, sl_rng, ioff_gl, inod)
!!      subroutine set_sleeve_node_ymin                                 &
!!     &         (c_size, nb_rng, sl_rng, ioff_gl, inod)
!!      subroutine set_sleeve_node_ymax                                 &
!!     &         (c_size, nb_rng, sl_rng, ioff_gl, inod)
!!        type(neib_range_cube), intent(in) :: nb_rng
!!        type(slleve_range), intent(in) :: sl_rng
!
      module sleeve_nod_side_cube
!
      use m_precision
!
      use t_size_of_cube
      use t_neib_range_cube
      use t_sleeve_cube
      use m_size_4_plane
      use m_cube_position
      use m_local_node_id_cube
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
      subroutine set_sleeve_node_xmin                                   &
     &         (c_size, nb_rng, sl_rng, ioff_gl, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl
      integer (kind = kint), intent(inout) :: inod
!
      integer (kind= kint) :: node_id_gl
      integer (kind= kint) :: i, j, k
      real (kind= kreal) :: x, y, z
!
!
      do k = sl_rng%ks, sl_rng%ke
       do j = sl_rng%js, sl_rng%je
        do i = 1, c_size%ndepth

         inod = inod + 1

         node_id_lc(i,j,k) =  inod
         node_id_gl = ioff_gl + i + (j + nb_rng%joff-1) * c_size%ndepth &
     &               + (k + nb_rng%koff-1) * c_size%ndepth              &
     &                * c_size%ny_all

         x = c_size%xmin + (i-1) * c_size%xsize / dble(c_size%nx_all)
         y = nb_rng%yoff + (j-1) * c_size%ysize / dble(c_size%ny_all)
         z = zz(nb_rng%koff + k)

         write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
        end do
       end do
      end do
!
      end subroutine set_sleeve_node_xmin
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_node_xmax                                   &
     &         (c_size, nb_rng, sl_rng, ioff_gl, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl
      integer (kind = kint), intent(inout) :: inod

!
      integer (kind= kint) :: node_id_gl
      integer (kind= kint) :: i, j, k, i1
      real (kind= kreal) :: x, y, z
!
!
      do k = sl_rng%ks, sl_rng%ke
       do j = sl_rng%js, sl_rng%je
        do i = 1, c_size%ndepth

         inod = inod + 1

         i1 = c_size%nxi + c_size%ndepth + i
         node_id_lc(i1,j,k) =  inod
         node_id_gl = ioff_gl + i + (j + nb_rng%joff-1) * c_size%ndepth &
     &            + (k + nb_rng%koff-1) * c_size%ndepth * c_size%ny_all

         x = c_size%xmax + (i+c_size%ndepth-1)                          &
     &                    * c_size%xsize / dble(c_size%nx_all)
         y = nb_rng%yoff + (j-1) * c_size%ysize / dble(c_size%ny_all)
         z = zz(nb_rng%koff + k)

         write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
        end do
       end do
      end do
!
      end subroutine set_sleeve_node_xmax
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_node_ymin                                   &
     &         (c_size, nb_rng, sl_rng, ioff_gl, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl
      integer (kind = kint), intent(inout) :: inod

!
      integer (kind= kint) :: node_id_gl
      integer (kind= kint) :: i, j, k
      real (kind= kreal) :: x, y, z
!
!
      do k = sl_rng%ks, sl_rng%ke
       do j = 1, c_size%ndepth
        do i = sl_rng%is, sl_rng%ie

         inod = inod + 1

         node_id_lc(i,j,k) =  inod
         node_id_gl = ioff_gl                                           &
     &               + (nb_rng%ioff + i) + (j-1) * c_size%nx_all        &
     &               + (nb_rng%koff + k-1) * c_size%nx_all              &
     &                * c_size%ndepth

         x = nb_rng%xoff + (i-1) * c_size%xsize / dble(c_size%nx_all)
         y = c_size%ymin + (j-1) * c_size%ysize / dble(c_size%ny_all)
         z = zz(nb_rng%koff + k)

         write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
        end do
       end do
      end do
!
      end subroutine set_sleeve_node_ymin
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_node_ymax                                   &
     &         (c_size, nb_rng, sl_rng, ioff_gl, inod)
!
      type(size_of_cube), intent(in) :: c_size
      type(neib_range_cube), intent(in) :: nb_rng
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl
      integer (kind = kint), intent(inout) :: inod
!
      integer (kind= kint) :: node_id_gl
      integer (kind= kint) :: i, j, k, j1
      real (kind= kreal) :: x, y, z
!
!
      do k = sl_rng%ks, sl_rng%ke
       do j = 1, c_size%ndepth
        do i = sl_rng%is, sl_rng%ie

         inod = inod + 1
         j1 = c_size%nyi + c_size%ndepth + j

         node_id_lc(i,j1,k) =  inod
         node_id_gl = ioff_gl                                           &
     &               + (nb_rng%ioff + i) + (j-1) * c_size%nx_all        &
     &               + (nb_rng%koff + k-1) * c_size%nx_all              &
     &                * c_size%ndepth 

         x = nb_rng%xoff + (i-1) * c_size%xsize / dble(c_size%nx_all)
         y = c_size%ymax + (j+c_size%ndepth-1)                          &
     &                    * c_size%ysize / dble(c_size%ny_all)
         z = zz(nb_rng%koff + k)

         write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
        end do
       end do
      end do
!
      end subroutine set_sleeve_node_ymax
!
!  ---------------------------------------------------------------------
!
      end module sleeve_nod_side_cube
