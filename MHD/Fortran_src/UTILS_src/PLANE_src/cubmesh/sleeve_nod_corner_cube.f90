!sleeve_nod_corner_cube.f90
!      module sleeve_nod_corner_cube
!
!!      subroutine set_sleeve_node_xmin_ymin                            &
!!     &         (sl_rng, ioff_gl, koff, inod)
!!      subroutine set_sleeve_node_xmax_ymin                            &
!!     &         (sl_rng, ioff_gl, koff, inod)
!!      subroutine set_sleeve_node_xmax_ymax                            &
!!     &         (sl_rng, ioff_gl, koff, inod)
!!      subroutine set_sleeve_node_xmin_ymax                            &
!!     &         (sl_rng, ioff_gl, koff, inod)
!!        type(slleve_range), intent(in) :: sl_rng
!
      module sleeve_nod_corner_cube
!
      use m_precision
!
      use t_sleeve_cube
      use m_local_node_id_cube
      use m_size_of_cube
      use m_size_4_plane
      use m_cube_position
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
     &         (sl_rng, ioff_gl, koff, inod)
!
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
       do j = 1, ndepth
        do i = 1, ndepth

         inod = inod + 1

         node_id_lc(i,j,k) =  inod
         node_id_gl        = ioff_gl + i + (j-1) * ndepth               &
     &                      + (koff+k-1)*ndepth*ndepth

         x = xmin + (i-1)*xsize/(nx_all)
         y = ymin + (j-1)*ysize/(ny_all)
         z = zz(koff+k)

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
     &         (sl_rng, ioff_gl, koff, inod)
!
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
       do j = 1, ndepth
        do i = 1, ndepth

         inod = inod + 1

         node_id_lc(nxi+ndepth+i,j,k) =  inod
         node_id_gl        = ioff_gl + i + (j-1) * ndepth               &
     &                      + (koff+k-1)*ndepth*ndepth

         x = xmax + (i+ndepth-1)*xsize/(nx_all)
         y = ymin + (j-1)*ysize/(ny_all)
         z = zz(koff+k)

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
     &         (sl_rng, ioff_gl, koff, inod)
!
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl, koff
      integer (kind = kint), intent(inout) :: inod

!
      integer (kind= kint) :: node_id_gl
      integer (kind= kint) :: i, j, k
      real (kind= kreal) :: x, y, z
!
!
      do k = sl_rng%ks, sl_rng%ke
       do j = 1, ndepth
        do i = 1, ndepth

         inod = inod + 1

         node_id_lc(nxi+ndepth+i,nyi+ndepth+j,k) = inod
         node_id_gl        = ioff_gl + i + (j-1) * ndepth               &
     &                      + (koff+k-1)*ndepth*ndepth

         x = xmax + (i+ndepth-1)*xsize/(nx_all)
         y = ymax + (j+ndepth-1)*ysize/(ny_all)
         z = zz(koff+k)

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
     &         (sl_rng, ioff_gl, koff, inod)
!
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl, koff
      integer (kind = kint), intent(inout) :: inod

!
      integer (kind= kint) :: node_id_gl
      integer (kind= kint) :: i, j, k
      real (kind= kreal) :: x, y, z
!
!
      do k = sl_rng%ks, sl_rng%ke
       do j = 1, ndepth
        do i = 1, ndepth

         inod = inod + 1

         node_id_lc(i,nyi+ndepth+j,k) = inod
         node_id_gl        = ioff_gl + i + (j-1) * ndepth               &
     &                      + (koff+k-1)*ndepth*ndepth

         x = xmin + (i-1)*xsize/(nx_all)
         y = ymax + (j+ndepth-1)*ysize/(ny_all)
         z = zz(koff+k)

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
