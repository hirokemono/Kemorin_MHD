!
      module m_sleeve_nod_side_cube
!
      use m_precision
!
      use m_size_of_cube
      use m_size_4_plane
      use m_cube_position
      use m_local_node_id_cube
      use m_offset_size_cube
      use m_cube_files_data
      use m_sleeve_cube
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
!      subroutine set_sleeve_node_xmin
!
      subroutine set_sleeve_node_xmin(inod, ioff_gl)

!
!      Written by Kemorin
!
      use m_precision
!
      implicit none
!
      integer (kind = kint) :: ioff_gl
      integer (kind = kint) :: inod
!
      integer (kind= kint) :: node_id_gl
      integer (kind= kint) :: i, j, k
      real (kind= kreal) :: x, y, z
!
!
      do k = sl_rng1%ks, sl_rng1%ke
       do j = sl_rng1%js, sl_rng1%je
        do i = 1, ndepth

         inod = inod + 1

         node_id_lc(i,j,k) =  inod
         node_id_gl = ioff_gl + i + (j+joff-1) * ndepth                 &
     &                + (k+koff-1) * ndepth * ny_all 

         x = xmin + (i-1)*xsize/(nx_all)
         y = yoff + (j-1)*ysize/(ny_all)
         z = zz(koff+k)

         write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
        end do
       end do
      end do
!
      end subroutine set_sleeve_node_xmin
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_node_xmax(inod, ioff_gl)

!
!      Written by Kemorin
!
!
      integer (kind = kint) :: ioff_gl
      integer (kind = kint) :: inod

!
      integer (kind= kint) :: node_id_gl
      integer (kind= kint) :: i, j, k
      real (kind= kreal) :: x, y, z
!
!
      do k = sl_rng1%ks, sl_rng1%ke
       do j = sl_rng1%js, sl_rng1%je
        do i = 1, ndepth

         inod = inod + 1

         node_id_lc(nxi+ndepth+i,j,k) =  inod
         node_id_gl = ioff_gl + i + (j+joff-1) * ndepth                 &
     &                + (k+koff-1) * ndepth * ny_all 

         x = xmax + (i+ndepth-1)*xsize/(nx_all)
         y = yoff + (j-1)*ysize/(ny_all)
         z = zz(koff+k)

         write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
        end do
       end do
      end do
!
      end subroutine set_sleeve_node_xmax
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_node_ymin(inod, ioff_gl)

!
!      Written by Kemorin
!
      integer (kind = kint) :: ioff_gl
      integer (kind = kint) :: inod

!
      integer (kind= kint) :: node_id_gl
      integer (kind= kint) :: i, j, k
      real (kind= kreal) :: x, y, z
!
!
      do k = sl_rng1%ks, sl_rng1%ke
       do j = 1, ndepth
        do i = sl_rng1%is, sl_rng1%ie

         inod = inod + 1

         node_id_lc(i,j,k) =  inod
         node_id_gl = ioff_gl + (ioff+i  ) + (j-1) * nx_all            &
     &               + (koff+k-1)*nx_all*ndepth 

         x = xoff + (i-1)*xsize/(nx_all)
         y = ymin + (j-1)*ysize/(ny_all)
         z = zz(koff+k)

         write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
        end do
       end do
      end do
!
      end subroutine set_sleeve_node_ymin
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_node_ymax(inod, ioff_gl)

!
!      Written by Kemorin
!
!
      integer (kind = kint) :: ioff_gl
      integer (kind = kint) :: inod

!
      integer (kind= kint) :: node_id_gl
      integer (kind= kint) :: i, j, k
      real (kind= kreal) :: x, y, z
!
!
      do k = sl_rng1%ks, sl_rng1%ke
       do j = 1, ndepth
        do i = sl_rng1%is, sl_rng1%ie

         inod = inod + 1

         node_id_lc(i,nyi+ndepth+j,k) =  inod
         node_id_gl = ioff_gl + (ioff+i  ) + (j-1) * nx_all            &
     &               + (koff+k-1)*nx_all*ndepth 

         x = xoff + (i-1)*xsize/(nx_all)
         y = ymax + (j+ndepth-1)*ysize/(ny_all)
         z = zz(koff+k)

         write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
        end do
       end do
      end do
!
      end subroutine set_sleeve_node_ymax
!
!  ---------------------------------------------------------------------
!
      end module m_sleeve_nod_side_cube
