!m_sleeve_edge_corner_cube.f90
!     module m_sleeve_edge_corner_cube
!
      module m_sleeve_edge_corner_cube
!
!      Written by Kemorin
!
      use m_precision
!
      use m_size_of_cube
      use m_size_4_plane
      use m_offset_size_cube
      use m_sleeve_cube
      use m_local_node_id_cube
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
!      subroutine set_sleeve_edge_xmin_ymin
!
       subroutine set_sleeve_edge_xmin_ymin(kpe, knp,                   &
     &          inod, ioff_gl, nd)

!
       integer (kind = kint) :: ioff_gl
       integer (kind = kint) :: kpe, jnp, knp
       integer (kind = kint) :: inod
       integer (kind = kint) :: nd
!
       integer (kind = kint) :: node_id_gl
       integer (kind = kint) :: i, j, k
       integer (kind = kint) :: ke1
       real (kind = kreal) :: x, y, z
       real (kind = kreal), parameter :: half = 0.5d0
!
       is = 1
       ie = ndepth
       js = 1
       je = ndepth
       ke1 = ke
       if ( nd.eq.3 .and. knp.gt.0) ke1 = ke1-1
       if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0 ) ke1 = ke1-1
!
       do k = ks, ke1
        do j = js, je
         do i = is, ie

          inod = inod + 1

          edge_id_lc(i,j,k,nd) =  inod
          node_id_gl = ioff_gl + i + (j-1) * ndepth                     &
     &                + (koff+k-1) * ndepth * ndepth

          if (nd .eq. 1) then
           x = xmin + ( dble(i)-half )*xsize / dble(nx_all)
           y = ymin + dble(j-1) *      ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 2) then
           x = xmin + dble(i-1) *      xsize / dble(nx_all)
           y = ymin + ( dble(j)-half )*ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 3) then
           x = xmin + dble(i-1) *      xsize / dble(nx_all)
           y = ymin + dble(j-1) *      ysize / dble(ny_all)
           z = zz_edge(koff+k)
          end if

          if ( iflag_data_f .eq. 1) then
           write(l_out) node_id_gl, x, y, z
          else
           write(l_out,'(i10,3(1pe21.11))') node_id_gl, x, y, z
          endif

         enddo
        enddo
       enddo
!
       end subroutine set_sleeve_edge_xmin_ymin
!
!  ---------------------------------------------------------------------
!
!
!      subroutine set_sleeve_edge_xmax_ymin
!
       subroutine set_sleeve_edge_xmax_ymin(kpe, knp,                   &
     &          inod, ioff_gl, nd)

!
       integer (kind = kint) :: ioff_gl
       integer (kind = kint) :: kpe, jnp, knp
       integer (kind = kint) :: inod
       integer (kind = kint) :: nd
!
       integer (kind = kint) :: node_id_gl
       integer (kind = kint) :: i, j, k
       integer (kind = kint) :: ie1, ke1
       real (kind = kreal) :: x, y, z
       real (kind = kreal), parameter :: half = 0.5d0
!
       is = 1
       ie = ndepth
       js = 1
       je = ndepth
       ie1 = ie
       ke1 = ke
       if ( nd.eq.1 ) ie1 = ie1 - 1
       if ( nd.eq.3 .and. knp.gt.0) ke1 = ke1-1
       if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0 ) ke1 = ke1-1
!
       do k = ks, ke1
        do j = js, je
         do i = is, ie1

          inod = inod + 1

          edge_id_lc(nxi+ndepth+i,j,k,nd) =  inod
          node_id_gl = ioff_gl + i + (j-1) * (ie1-is+1)                 &
     &                + (koff+k-1) * (ie1-is+1) * ndepth

          if (nd .eq. 1) then
           x = xmax + ( dble(i+ndepth)-half )*xsize / dble(nx_all)
           y = ymin + dble(j-1) *             ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 2) then
           x = xmax + ( dble(i+ndepth-1) )*xsize / dble(nx_all)
           y = ymin + ( dble(j)-half )*ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 3) then
           x = xmax + ( dble(i+ndepth-1) )*xsize / dble(nx_all)
           y = ymin + dble(j-1) *      ysize / dble(ny_all)
           z = zz_edge(koff+k)
          end if

          if ( iflag_data_f .eq. 1) then
           write(l_out) node_id_gl, x, y, z
          else
           write(l_out,'(i10,3(1pe21.11))') node_id_gl, x, y, z
          endif

         enddo
        enddo
       enddo
!
       end subroutine set_sleeve_edge_xmax_ymin
!
!  ---------------------------------------------------------------------
!
!      subroutine set_sleeve_edge_xmax_ymax
!
       subroutine set_sleeve_edge_xmax_ymax(kpe, knp,                   &
     &          inod, ioff_gl, nd)

!
       integer (kind = kint) :: ioff_gl
       integer (kind = kint) :: kpe, jnp, knp
       integer (kind = kint) :: inod
       integer (kind = kint) :: nd

!
       integer (kind = kint) :: node_id_gl
       integer (kind = kint) :: i, j, k
       integer (kind = kint) ::ie1, je1, ke1
       real (kind = kreal) :: x, y, z
       real (kind = kreal), parameter :: half = 0.5d0
!
       is = 1
       ie = ndepth
       js = 1
       je = ndepth
       ie1 = ie
       je1 = je
       ke1 = ke
       if ( nd.eq.1 ) ie1 = ie1 - 1
       if ( nd.eq.2 ) je1 = je1 - 1
       if ( nd.eq.3 .and. knp.gt.0) ke1 = ke1-1
       if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0 ) ke1 = ke1-1
!
       do k = ks, ke1
        do j = js, je1
         do i = is, ie1

          inod = inod + 1

          edge_id_lc(nxi+ndepth+i,nyi+ndepth+j,k,nd) =  inod
          node_id_gl = ioff_gl + i + (j-1) * (ie1-is+1)                 &
     &                + (koff+k-1) * (ie1-is+1) * (je1-js+1)

          if (nd .eq. 1) then
           x = xmax + ( dble(i+ndepth)-half ) * xsize / dble(nx_all)
           y = ymax + dble(ndepth+j-1) *        ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 2) then
           x = xmax + dble(i+ndepth-1) *        xsize / dble(nx_all)
           y = ymax + ( dble(ndepth+j)-half ) * ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 3) then
           x = xmax + dble(i+ndepth-1) *        xsize / dble(nx_all)
           y = ymax + dble(ndepth+j-1) *        ysize / dble(ny_all)
           z = zz_edge(koff+k)
          end if

          if ( iflag_data_f .eq. 1) then
           write(l_out) node_id_gl, x, y, z
          else
           write(l_out,'(i10,3(1pe21.11))') node_id_gl, x, y, z
          endif

         enddo
        enddo
       enddo
!
       end subroutine set_sleeve_edge_xmax_ymax
!
!  ---------------------------------------------------------------------
!
!      subroutine set_sleeve_edge_xmin_ymax
!
       subroutine set_sleeve_edge_xmin_ymax(kpe, knp,                   &
     &           inod, ioff_gl, nd)
!
!
       integer (kind = kint) :: ioff_gl
       integer (kind = kint) :: kpe, jnp, knp
       integer (kind = kint) :: inod
       integer (kind = kint) :: nd

!
       integer (kind = kint) :: node_id_gl
       integer (kind = kint) :: i, j, k
       integer (kind = kint) ::je1, ke1
       real (kind = kreal) :: x, y, z
       real (kind = kreal), parameter :: half = 0.5d0
!
       is = 1
       ie = ndepth
       js = 1
       je = ndepth
       je1 = je
       ke1 = ke
       if ( nd.eq.2 ) je1 = je1 - 1
       if ( nd.eq.3 .and. knp.gt.0) ke1 = ke1-1
       if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0 ) ke1 = ke1-1
!
       do k = ks, ke1
        do j = js, je1
         do i = is, ie

          inod = inod + 1

          edge_id_lc(i,nyi+ndepth+j,k,nd) =  inod
          node_id_gl = ioff_gl + i + (j-1) * ndepth                     &
     &                + (koff+k-1) * ndepth * (je1-js+1)

          if (nd .eq. 1) then
           x = xmin + ( dble(i)-half ) *        xsize / dble(nx_all)
           y = ymax + dble(ndepth+j-1) *        ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 2) then
           x = xmin + dble(i-1) *               xsize / dble(nx_all)
           y = ymax + ( dble(ndepth+j)-half ) * ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 3) then
           x = xmin + dble(i-1) *               xsize / dble(nx_all)
           y = ymax + dble(ndepth+j-1) *        ysize / dble(ny_all)
           z = zz_edge(koff+k)
          end if

          if ( iflag_data_f .eq. 1) then
           write(l_out) node_id_gl, x, y, z
          else
           write(l_out,'(i10,3(1pe21.11))') node_id_gl, x, y, z
          endif

         enddo
        enddo
       enddo
!
       end subroutine set_sleeve_edge_xmin_ymax
!
      end module m_sleeve_edge_corner_cube
!
