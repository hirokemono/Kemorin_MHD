!write_z_filter_4_nod.f90
!      module write_z_filter_4_nod
!
      module write_z_filter_4_nod
!
!      Written by H. Matsui
!
      use m_precision
!
      implicit none
!
!      subroutine write_filter_4_nod
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine write_filter_4_nod
!
      use m_geometry_parameter
      use m_geometry_data
      use m_int_commtative_filter
      use m_commute_filter_z
      use m_filter_values
      use m_int_edge_vart_width
!
      integer (kind= kint), parameter :: id_filter_z = 15
      integer (kind= kint) :: i, inod, iele, j, k, kf
!
!
      open (id_filter_z,file=filter_z_file_name)
!
        write(id_filter_z,*) '! number of node'
        write(id_filter_z,*) totalnod_x, totalnod_y, internal_node
        write(id_filter_z,*) '! size of domain'
        write(id_filter_z,'(1p3E25.15e3)') xsize, ysize, zsize
        write(id_filter_z,*) '!grid type'
        write(id_filter_z,*) '!   0:equally divided'
        write(id_filter_z,*) '!   1:Chebycyev points from 0 to pi/2'
        write(id_filter_z,*) '!   2:Chebycyev points from 0 to pi'
          write(id_filter_z,*) iflag_grid
        write(id_filter_z,*) '! width of nodes for filtering'
        write(id_filter_z,*) ncomp_mat, ncomp_mat, ncomp_mat
!
        write(id_filter_z,*) '! filter_type for horizontal direction'
        if ( iflag_filter_h.eq.0 ) then
          write(id_filter_z,*) 'Top_hat'
        else if ( iflag_filter_h.eq.1 ) then
          write(id_filter_z,*) 'Linear'
        else if ( iflag_filter_h.eq.2 ) then
          write(id_filter_z,*) 'Gaussian'
        end if
        write(id_filter_z,*) '! filter_type for z direction'
        if ( iflag_filter.eq.0 ) then
          write(id_filter_z,*) 'Top_hat'
        else if ( iflag_filter.eq.1 ) then
          write(id_filter_z,*) 'Linear'
        else if ( iflag_filter.eq.2 ) then
          write(id_filter_z,*) 'Gaussian'
        end if
        write(id_filter_z,*) '! Filter width( vertical, horizontal)'
        write(id_filter_z,*) f_width, f_width_h
!
!
      do i = 1, ncomp_norm
       do j = 0, 2
        if ( kcomp_norm(i) .eq. j) then
          xmom_ht_z(j) = f_mom(i)
        end if
       end do
      end do
!
        write(id_filter_z,*) '! origianl moments for three directions'
        write(id_filter_z,'(1p3E25.15e3)') (xmom_ht_x(kf), kf=0,2)
        write(id_filter_z,'(1p3E25.15e3)') (xmom_ht_y(kf), kf=0,2)
        write(id_filter_z,'(1p3E25.15e3)') (xmom_ht_z(kf), kf=0,2)
!
!        write(id_filter_z,*) '! moments for x-direction'
!        write(id_filter_z,'(1p3E25.15e3)') (xmom_ht_x(kf), kf=0,2)
        write(id_filter_z,*) '! coefficients for x-direction of moments'
        do j=1, ncomp_mat
          write(id_filter_z,'(i5,1p50E25.15e3)') j,                     &
     &            (xmom_h_x(j,kf), kf=0,2)
        end do
!
!        write(id_filter_z,*) '! moments for y-direction'
!        write(id_filter_z,'(1p3E25.15e3)') (xmom_ht_y(kf), kf=0,2)
        write(id_filter_z,*) '! coefficients for y-direction of moments'
        do j=1, ncomp_mat
          write(id_filter_z,'(i5,1p50E25.15e3)') j,                     &
     &            (xmom_h_y(j,kf), kf=0,2)
        end do
!
        write(id_filter_z,*) '! node_id, z, dz/dzeta, diff of delta_z'
!
        do inod = 1, internal_node
          write(id_filter_z,'(i16,1p4E25.15e3)') inod_global(inod),     &
     &      xx(inod,3), delta_z(inod), delta_dz(inod), d2_dz(inod)
        end do
!
        write(id_filter_z,*)                                            &
     &       '! element_id, connectivity, dz/dzeta, diff of delta_z'
!
        do iele = 1, numele
          write(id_filter_z,'(3i16,1p3E25.15e3)') iedge_global(iele),   &
     &          ie_edge(iele,1), ie_edge(iele,2), delta_z_e(iele),      &
     &          delta_dz_e(iele), d2_dz_e(iele)
        end do
!
!
!
        write(id_filter_z,*) '! node_id, neighboring_nodes'
!
        do inod = 1, internal_node
          write(id_filter_z,'(20i16)')                                  &
     &      inod_global(inod), (nneib_nod2(inod,i),i=1,2),              &
     &     (inod_global(inod+i-nneib_nod2(inod,1)-1),i=1,ncomp_mat)
        end do
!
!
        write(id_filter_z,*) '! node_id, coefs of filter'
!
        do inod = 1, internal_node
          write(id_filter_z,'(i16,1p20E25.15e3)') inod_global(inod),    &
     &          (c_filter(j,inod),j=1,ncomp_mat)
        end do
!
!
         write(id_filter_z,*) '! node_id, 1d_momentum on node (modified)'
!
        do inod = 1, internal_node
          write(id_filter_z,'(i16,1p6E25.15e3)') inod_global(inod),     &
     &          (xmom_int_t(inod,k),k=0,2)
        end do
!
         write(id_filter_z,*)                                           &
     &         '! node_id, diff. of 1d_momentum on node (modified)'
!
        do inod = 1, internal_node
          write(id_filter_z,'(i16,1p6E25.15e3)') inod_global(inod),     &
     &          (xmom_dt(inod,k),k=0,2)
        end do
!
!
      do k = 0, 2
         write(id_filter_z,*) '! node_id, coefs_4_filtering (modified)'
!
!
        do inod = 1, internal_node
          write(id_filter_z,'(2i16,1p20E25.15e3)') k,                   &
     &         inod_global(inod), (xmom_int(inod,j,k),j=1,ncomp_mat)
        end do
      end do
!
         write(id_filter_z,*) '! node_id, 1d_momentum on node (original)'
!
        do inod = 1, internal_node
          write(id_filter_z,'(i16,1p6E25.15e3)') inod_global(inod),     &
     &          (xmom_int_to(inod,k),k=0,2)
        end do
!
         write(id_filter_z,*)                                           &
     &         '! node_id, diff. of 1d_momentum on node (original)'
!

        do inod = 1, internal_node
          write(id_filter_z,'(i16,1p6E25.15e3)') inod_global(inod),     &
     &          (xmom_dot(inod,k),k=0,2)
        end do
!
      do k = 0, 2
         write(id_filter_z,*) '! node_id, coefs_4_filtering (original)'
!
!
        do inod = 1, internal_node
          write(id_filter_z,'(2i16,1p20E25.15e3)') k,                   &
     &        inod_global(inod), (xmom_int_org(inod,j,k),j=1,ncomp_mat)
        end do
      end do
!
      close(id_filter_z)
!
      end subroutine write_filter_4_nod
!
!   --------------------------------------------------------------------
!
      end module write_z_filter_4_nod
