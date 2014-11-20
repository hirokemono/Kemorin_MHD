!
!      module m_neib_nod_cube
!
      module m_neib_nod_cube
!
!     written by H. Matsui
!
!  ----------------------------------------------------------------------
!
!      binary data data format
!        Number of node
!        Number of depth, max. number of nodes for filtering
!        Stack for filtering (1 to num. of node)
!        Local node ID for filtering
!        distance in xi-direction
!        distance in eta-direction
!        distance in zta-direction
!
!  ----------------------------------------------------------------------
!
      use m_precision
!
      use m_size_of_cube
      use m_offset_size_cube
      use m_local_node_id_cube
      use m_neighb_range_cube
      use m_cube_files_data
      use m_filtering_nod_4_cubmesh
      use m_neib_nod_line_cube
      use m_filter_data_4_plane
!
      use m_filter_elength
      use set_parallel_file_name
!
      implicit none
!
      integer(kind = kint) :: i_st2, i_end2
      integer(kind = kint) :: j_st2, j_end2
      integer(kind = kint) :: k_st2, k_end2
      private :: i_st2, i_end2, j_st2, j_end2, k_st2, k_end2
!
!  ----------------------------------------------------------------------
!
      contains
!
!  ----------------------------------------------------------------------
!
      subroutine neighboring_node(pe_id, kpe)
!
      use set_parallel_file_name
!
        integer(kind = kint) :: kpe, pe_id
        integer(kind = kint) :: pe1
!
        pe1 = pe_id - 1
!
        write(*,*) 'allocate_filters_nod'
       call allocate_filters_nod
!
       i_st2 =  max(i_st-ndepth,1)
       i_end2 = min(i_end+ndepth,nx)
       j_st2 =  max(j_st-ndepth,1)
       j_end2 = min(j_end+ndepth,ny)
       k_st2 =  max(k_st-ndepth,1)
       k_end2 = min(k_end+ndepth,nz)
!
        write(*,*) 'i_st, i_end', i_st2, i_end2, nx
        write(*,*) 'j_st, j_end', j_st2, j_end2, ny
        write(*,*) 'k_st, k_end', k_st2, k_end2, nz
!
        write(*,*) 'set_range_4_nodeloop'
        call set_range_4_nodeloop(kpe)
!
        nnod_filter_mom = nodtot
        nele_filter_mom = elmtot
        call allocate_ref_1d_moment
        call allocate_ele_length
!
        write(*,*) 'set_element_size_on_nod'
        call set_element_size_on_nod
        call set_element_size_on_ele
!
        write(*,*) 'count_neib_node_x'
        call count_neib_node_x
!        call check_neib_node_x
        write(*,*) 'count_neib_node_y'
        call count_neib_node_y
!        call check_neib_node_y
        write(*,*) 'count_neib_node_z'
        call count_neib_node_z
        if (iflag_z_filter.eq.0)  then
          write(*,*) 'norm_z_coefs'
          call norm_z_coefs
        end if
!        call check_neib_node_z
!
        if ( iflag_data_f .eq. 1) then
          call add_int_suffix(pe1, filter_file_header, nb_name)
          write(*,*) 'output binary file: ', nb_name
          open (nb_out, file=nb_name, form='unformatted')
          call write_neighboring_nod_line_b
        else
          call add_int_suffix(pe1, filter_file_header, nb_name)
          write(*,*) 'output ascii file: ', trim(nb_name)
          open (nb_out, file=nb_name)
          call write_neighboring_nod_line
        end if
        close(nb_out)
!
        write(*,*) 'deallocate_filters_nod'
        call deallocate_filters_nod
!
        call deallocate_ele_length
        call deallocate_ref_1d_moment
!
       end subroutine neighboring_node
!
!  ---------------------------------------------------------------------
!
       subroutine set_element_size_on_nod
!
       integer(kind = kint) :: nd, ifil, kf
!
!
       if (nf_type.eq.0) return
!
       do ifil = 1, nf_type
         filter_type(ifil) = filtertype_z(ifil)
         f_width(ifil) = width_f(ifil)
           do kf = 0, 2
             xmom_1d_org(ifil,kf) = mom_1d_o(kf,3,ifil)
           end do
       end do
!
       if (iflag_z_filter.eq.0) then
         do ifil = 1, nf_type
           filter_type(ifil) = filtertype_h(ifil)
         end do
       end if
!
!
       end subroutine set_element_size_on_nod
!
!
!  ---------------------------------------------------------------------
!
       subroutine set_element_size_on_ele
!
       integer(kind = kint) :: i, j, k, iele, k_gl, nd
!
!
       if (nf_type.eq.0) return
!
       iele = 0
       do k=1,nz-1
         k_gl = k + koff
         do j=1,ny-1
           do i=1,nx-1
!
             iele = iele + 1
!
             elen_dx2_ele(iele) = delta_h(1) * delta_h(1)
             elen_dy2_ele(iele) = delta_h(2) * delta_h(2)
             elen_dz2_ele(iele) = delta_z_e(k_gl) * delta_z_e(k_gl)
             elen_dxdy_ele(iele) = 0.0d0
             elen_dydz_ele(iele) = 0.0d0
             elen_dzdx_ele(iele) = 0.0d0
             do nd = 1, 3
               elen_dx2_ele_dx(iele,nd) =   0.0d0
               elen_dy2_ele_dx(iele,nd) =   0.0d0
               elen_dxdy_ele_dx(iele,nd) =  0.0d0
               elen_dydz_ele_dx(iele,nd) =  0.0d0
               elen_dzdx_ele_dx(iele,nd) =  0.0d0
               elen_dx2_ele_dx2(iele,nd) =  0.0d0
               elen_dy2_ele_dx2(iele,nd) =  0.0d0
               elen_dxdy_ele_dx2(iele,nd) = 0.0d0
               elen_dydz_ele_dx2(iele,nd) = 0.0d0
               elen_dzdx_ele_dx2(iele,nd) = 0.0d0
             end do
             elen_dz2_ele_dx(iele,1) = 0.0d0
             elen_dz2_ele_dx(iele,2) = 0.0d0
             elen_dz2_ele_dx(iele,3) = 2.0d0 * delta_z_e(k_gl)          &
     &                                       * diff_deltaz_e(k_gl)
             elen_dz2_ele_dx2(iele,1) = 0.0d0
             elen_dz2_ele_dx2(iele,2) = 0.0d0
             elen_dz2_ele_dx2(iele,3) = 2.0d0 * d2_deltaz_e(k_gl)       &
     &                                 + 2.0d0 * diff_deltaz_e(k_gl)    &
     &                                         * diff_deltaz_e(k_gl)
           enddo
         enddo
       enddo
!
       end subroutine set_element_size_on_ele
!
!  ---------------------------------------------------------------------
!
       subroutine count_neib_node_x
!
       integer(kind = kint) :: i, j, k
       integer(kind = kint) :: ii, i1, ifil, i2
       integer(kind = kint),dimension(-1:1) :: ndepth_x
!
!
       do k=k_st2,k_end2
        do j=j_st2,j_end2
         do i=i_st2, i_end2
          ndepth_x(-1) = i - max(i-ndepth,1)
          ndepth_x( 0) = 1
          ndepth_x( 1) = min(i+ndepth,nx) - i
!          if ( ndepth_x(-1) .lt. ndepth) then
!           ndepth_x( 1) = 2*ndepth - ndepth_x(-1)
!          end if
!          if ( ndepth_x( 1) .lt. ndepth) then
!           ndepth_x(-1) = 2*ndepth - ndepth_x( 1)
!          end if
!
          nnod_neib_x(i,j,k) = ndepth_x(-1) + ndepth_x(0) + ndepth_x(1)
!
          do ii = ndepth_x(-1), 1, -1
            i1 = ndepth_x(-1) - ii + 1
            i2 = nneib_h(1) - ii + 1
            inod_f_item_x(i1,i,j,k) = i - ii
            inod_f_dist_x(i1,i,j,k) = - ii
            do ifil = 1, nf_type
             filter_c_x(i1,i,j,k,ifil) = coef_nod_x(i2,0,ifil)
            end do
          end do
          i1 = ndepth_x(-1) + 1
          i2 = nneib_h(1)+1
          inod_f_item_x(i1,i,j,k) = i
          inod_f_dist_x(i1,i,j,k) = 0
          do ifil = 1, nf_type
            filter_c_x(i1,i,j,k,ifil) = coef_nod_x(i2,0,ifil)
          end do
          do ii = 1, ndepth_x( 1)
            i1 = ii + ndepth_x(-1) + 1
            i2 = nneib_h(1) + ii + 1
            inod_f_item_x(i1,i,j,k) = i + ii
            inod_f_dist_x(i1,i,j,k) = ii
            do ifil = 1, nf_type
             filter_c_x(i1,i,j,k,ifil) = coef_nod_x(i2,0,ifil)
            end do
          end do
!
         enddo
        enddo
       enddo
!
       end subroutine count_neib_node_x
!
!  ---------------------------------------------------------------------
!
       subroutine count_neib_node_y
!
       integer(kind = kint) :: i, j, k
       integer(kind = kint) :: jj, j1, ifil, j2
       integer(kind = kint),dimension(-1:1) :: ndepth_y
!
       nnod_neib_y = 0
!
       do k=k_st2, k_end2
        do j=j_st2, j_end2
         ndepth_y(-1) = j - max(j-ndepth,1)
         ndepth_y( 0) = 1
         ndepth_y( 1) = min(j+ndepth,ny) - j
 !        if ( ndepth_y(-1) .lt. ndepth) then
 !          ndepth_y( 1) = 2*ndepth - ndepth_y(-1)
 !        end if
 !        if ( ndepth_y( 1) .lt. ndepth) then
 !          ndepth_y(-1) = 2*ndepth - ndepth_y( 1)
 !        end if
         do i=i_st2, i_end2
!
          nnod_neib_y(i,j,k) = ndepth_y(-1) + ndepth_y(0) + ndepth_y(1)
!
          do jj = ndepth_y(-1), 1, -1
            j1 = ndepth_y(-1) - jj + 1
            j2 = nneib_h(2) - jj + 1
            inod_f_item_y(j1,i,j,k) = j - jj
            inod_f_dist_y(j1,i,j,k) = -jj
            do ifil = 1, nf_type
              filter_c_y(j1,i,j,k,ifil) = coef_nod_y(j2,0,ifil)
            end do
          end do
          j1 = ndepth_y(-1) + 1
          j2 = nneib_h(2) + 1
          inod_f_item_y(j1,i,j,k) = j
          inod_f_dist_y(j1,i,j,k) = 0
          do ifil = 1, nf_type
            filter_c_y(j1,i,j,k,ifil) = coef_nod_y(j2,0,ifil)
          end do
          do jj = 1, ndepth_y( 1)
            j1 = jj + ndepth_y(-1) + 1
            j2 = nneib_h(2) + jj + 1
            inod_f_item_y(j1,i,j,k) = j + jj
            inod_f_dist_y(j1,i,j,k) = jj
            do ifil = 1, nf_type
              filter_c_y(j1,i,j,k,ifil) = coef_nod_y(j2,0,ifil)
            end do
          end do
!
         enddo
        enddo
       enddo
!
!
       end subroutine count_neib_node_y
!
!  ---------------------------------------------------------------------
!
       subroutine count_neib_node_z
!
       integer(kind = kint) :: i, j, k
       integer(kind = kint) :: kk, k1, ifil, k2, k_gl
       integer(kind = kint),dimension(-1:1) :: ndepth_z
!
       nnod_neib_z = 0
!
       do k=k_st2,k_end2
        k_gl = k + koff
        ndepth_z(-1) = k - max(k-ndepth,1)
        ndepth_z( 0) = 1
        ndepth_z( 1) = min(k+ndepth,nz) - k
!        if ( ndepth_z(-1) .lt. ndepth) then
!          ndepth_z( 1) = 2*ndepth - ndepth_z(-1)
!        end if
!        if ( ndepth_z( 1) .lt. ndepth) then
!          ndepth_z(-1) = 2*ndepth - ndepth_z( 1)
!        end if
        do j=j_st2,j_end2
         do i=i_st2,i_end2

          nnod_neib_z(i,j,k) = ndepth_z(-1) + ndepth_z(0) + ndepth_z(1)
!
          do kk = ndepth_z(-1), 1, -1
            k1 = ndepth_z(-1) - kk + 1
            inod_f_item_z(k1,i,j,k) = k - kk
            inod_f_dist_z(k1,i,j,k) = -kk
            do ifil = 1, nf_type
              k2 = nneib_z(k_gl,1,ifil) - kk + 1
              if (iflag_z_filter.eq.0) then
                filter_c_z(k1,i,j,k,ifil) = coef_nod_x(k2,0,ifil)
              else
               filter_c_z(k1,i,j,k,ifil) = coef_nod_z(k_gl,k2,0,ifil)
              end if
            end do
          end do
          k1 = ndepth_z(-1) + 1
          inod_f_item_z(k1,i,j,k) = k
          inod_f_dist_z(k1,i,j,k) = 0
          do ifil = 1, nf_type
            k2 = nneib_z(k_gl,1,ifil) + 1
              if (iflag_z_filter.eq.0) then
                filter_c_z(k1,i,j,k,ifil) = coef_nod_x(k2,0,ifil)
              else
               filter_c_z(k1,i,j,k,ifil) = coef_nod_z(k_gl,k2,0,ifil)
              end if
          end do
          do kk = 1, ndepth_z( 1)
            k1 = kk + ndepth_z(-1) + 1
            inod_f_item_z(k1,i,j,k) = k + kk
            inod_f_dist_z(k1,i,j,k) = kk
            do ifil = 1, nf_type
              k2 = nneib_z(k_gl,1,ifil) + kk + 1
              if (iflag_z_filter.eq.0) then
                filter_c_z(k1,i,j,k,ifil) = coef_nod_x(k2,0,ifil)
              else
               filter_c_z(k1,i,j,k,ifil) = coef_nod_z(k_gl,k2,0,ifil)
              end if
            end do
          end do
!
         enddo
        enddo
!
       enddo
!
!
       end subroutine count_neib_node_z
!
!  ---------------------------------------------------------------------
!
       subroutine norm_z_coefs
!
       integer(kind = kint) :: i, j, k
       integer(kind = kint) :: k1, ifil
       real(kind = kreal) :: total_mom
!
       do k=k_st2,k_end2
        do j=j_st2,j_end2
         do i=i_st2,i_end2
!
          do ifil = 1, nf_type
!
           total_mom = 0.0d0
           do k1 = 1, nnod_neib_z(i,j,k)
             total_mom = total_mom + filter_c_z(k1,i,j,k,ifil)
           end do
           do k1 = 1, nnod_neib_z(i,j,k)
             filter_c_z(k1,i,j,k,ifil)                                  &
     &         = filter_c_z(k1,i,j,k,ifil) / total_mom
           end do
!
          end do
!
         enddo
        enddo
       enddo
!
       end subroutine norm_z_coefs
!
!  ---------------------------------------------------------------------
!
       subroutine check_neib_node_x
!
       integer(kind = kint) :: i, j, k
       integer(kind = kint) :: ii, i1
!
       write(50,*) 'nnod_neib_x'
       write(50,'(10i16)') nnod_neib_x
!
       write(50,*) 'filter information for x-direction'
       do k=k_st2,k_end2
        do j=j_st2,j_end2
         do i=i_st2, i_end2
          do ii = 1, nnod_neib_x(i,j,k)
           i1 = inod_f_item_x(ii,i,j,k)
          write(50,'(4i5,10i6)') k, j, i, ii,                           &
     &         node_id_lc(i,j,k), inod_f_item_x(ii,i,j,k),              &
     &         node_id_lc(i1,j,k), inod_f_dist_x(ii,i,j,k)
          end do
         enddo
        enddo
       enddo
!
       end subroutine check_neib_node_x
!
!  ---------------------------------------------------------------------
!
       subroutine check_neib_node_y
!
       integer(kind = kint) :: i, j, k
       integer(kind = kint) :: jj, j1
!
       write(50,*) 'nnod_neib_y'
       write(50,'(10i16)') nnod_neib_y
!
       write(50,*) 'filter information for y-direction'
       do k=k_st2,k_end2
        do j=j_st2,j_end2
         do i=i_st2, i_end2
          do jj = 1, nnod_neib_y(i,j,k)
           j1 = inod_f_item_y(jj,i,j,k)
          write(50,'(4i5,10i6)') k, j, i, jj,                           &
     &         node_id_lc(i,j,k), inod_f_item_y(jj,i,j,k),              &
     &         node_id_lc(i,j1,k), inod_f_dist_y(jj,i,j,k)
          end do
         enddo
        enddo
       enddo
!
       end subroutine check_neib_node_y
!
!
!  ---------------------------------------------------------------------
!
       subroutine check_neib_node_z
!
       integer(kind = kint) :: i, j, k
       integer(kind = kint) :: kk, k1
!
       write(50,*) 'nnod_neib_z'
       write(50,'(10i16)') nnod_neib_z
!
       write(50,*) 'filter information for z-direction'
       do k=k_st2,k_end2
        do j=j_st2,j_end2
         do i=i_st2, i_end2
          do kk = 1, nnod_neib_z(i,j,k)
           k1 = inod_f_item_z(kk,i,j,k)
          write(50,'(4i5,10i6)') k, j, i, kk,                           &
     &         node_id_lc(i,j,k), inod_f_item_z(kk,i,j,k),              &
     &         node_id_lc(i,j,k1), inod_f_dist_z(kk,i,j,k)
          end do
         enddo
        enddo
       enddo
!
       end subroutine check_neib_node_z
!
!  ---------------------------------------------------------------------
!
       subroutine check_neib_node_xy
!
       integer(kind = kint) :: i, j, k, ifil
       integer(kind = kint) :: ij, i1, j1
!
       write(50,*) 'nnod_neib_xy'
       write(50,'(10i16)') nnod_neib_xy
!
       write(50,*) 'filter information for each plane'
       do k=k_st2,k_end2
        do j=j_st2,j_end2
         do i=i_st2, i_end2
          do ij = 1, nnod_neib_xy(i,j,k)
           i1 = inod_f_item_xy(ij,i,j,k,1)
           j1 = inod_f_item_xy(ij,i,j,k,2)
          write(50,'(4i5,10i6)') k, j, i, ij, node_id_lc(i,j,k),        &
     &         inod_f_item_xy(ij,i,j,k,1), inod_f_item_xy(ij,i,j,k,2),  &
     &         node_id_lc(i1,j1,k), inod_f_dist_xy(ij,i,j,k,1),         &
     &         inod_f_dist_xy(ij,i,j,k,2)
          write(50,'(1p10E25.15e3)')                                    &
     &              (filter_c_xy(ij,i,j,k,ifil),ifil=1,nf_type)
          end do
         enddo
        enddo
       enddo
!
       end subroutine check_neib_node_xy
!
!  ---------------------------------------------------------------------
!
       subroutine check_neib_node_3d
!
       integer(kind = kint) :: i, j, k
       integer(kind = kint) :: ijk, i1, j1, k1, nd
!
       write(50,*) 'nnod_neib'
       write(50,'(10i16)') nnod_neib
!
       write(50,*) 'filter information for all direction'
       do k=k_st2,k_end2
        do j=j_st2,j_end2
         do i=i_st2, i_end2
          do ijk = 1, nnod_neib(i,j,k)
           i1 = inod_f_item_3d(ijk,i,j,k,1)
           j1 = inod_f_item_3d(ijk,i,j,k,2)
           k1 = inod_f_item_3d(ijk,i,j,k,3)
          write(50,'(4i5,10i6)') k, j, i, ijk, node_id_lc(i,j,k),       &
     &         (inod_f_item_3d(ijk,i,j,k,nd), nd = 1, 3),               &
     &         node_id_lc(i1,j1,k1),                                    &
     &         (inod_f_dist_3d(ijk,i,j,k,nd), nd= 1,3)
          end do
         enddo
        enddo
       enddo
!
       end subroutine check_neib_node_3d
!
!  ---------------------------------------------------------------------
!
      end module m_neib_nod_cube
