!m_sleeve_nod_corner_cube.f90
!      module m_sleeve_nod_corner_cube
!
      module m_sleeve_nod_corner_cube
!
      use m_precision
!
      use m_local_node_id_cube
      use m_size_of_cube
      use m_size_4_plane
      use m_offset_size_cube
      use m_sleeve_cube
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
!      subroutine set_sleeve_node_xmin_ymin
!
      subroutine set_sleeve_node_xmin_ymin(inod, ioff_gl)

!
!      Written by Kemorin
!
      integer (kind = kint) :: ioff_gl
      integer (kind = kint) :: inod
!
      integer (kind = kint) :: node_id_gl
      integer (kind = kint) :: i, j, k
      real (kind = kreal) :: x, y, z
!
!
      do k=ks,ke
       do j=1, ndepth
        do i=1, ndepth

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
!      subroutine set_sleeve_node_xmax_ymin
!
      subroutine set_sleeve_node_xmax_ymin(inod, ioff_gl)

!
!      Written by Kemorin
!
      integer (kind = kint) :: ioff_gl
      integer (kind = kint) :: inod

!
      integer (kind = kint) :: node_id_gl
      integer (kind = kint) :: i, j, k
      real (kind = kreal) :: x, y, z
!
!
      do k=ks,ke
       do j=1, ndepth
        do i=1, ndepth

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
!      subroutine set_sleeve_node_xmax_ymax
!
      subroutine set_sleeve_node_xmax_ymax(inod, ioff_gl)

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
      do k=ks,ke
       do j=1, ndepth
        do i=1, ndepth

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
!      subroutine set_sleeve_node_xmin_ymax
!
      subroutine set_sleeve_node_xmin_ymax(inod, ioff_gl)

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
      do k=ks,ke
       do j=1, ndepth
        do i=1, ndepth

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
      end module m_sleeve_nod_corner_cube
