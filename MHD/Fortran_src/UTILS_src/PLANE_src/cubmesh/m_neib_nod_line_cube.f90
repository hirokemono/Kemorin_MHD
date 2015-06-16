!
!      module m_neib_nod_line_cube
!
      module m_neib_nod_line_cube
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
      use m_constants
      use m_size_of_cube
      use m_local_node_id_cube
      use m_neighb_range_cube
      use m_cube_files_data
      use m_filtering_nod_4_cubmesh
      use m_filter_data_4_plane
!
      use m_filter_elength
      use m_l_filtering_data
!
      implicit none
!
!
      integer(kind = kint), dimension(:,:), allocatable                 &
     &      :: inod_f_dist_l
!
      integer(kind = kint), dimension(:,:), allocatable                 &
     &      :: inod_4_l_filter_0
      integer(kind = kint), dimension(:,:), allocatable                 &
     &      :: istack_l_filter_0
!
      integer(kind = kint), dimension(:,:), allocatable                 &
     &      :: item_l_filter_0
      integer(kind = kint), dimension(:,:), allocatable                 &
     &      :: inod_f_dist_l_0
      real(kind = kreal), dimension(:,:), allocatable                   &
     &      :: coef_l_filter_0
!
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
!  ----------------------------------------------------------------------!
       subroutine allocate_neighboring_nod_line
!
       allocate( inod_f_dist_l(ndep_1*nodtot,3) )
!
       allocate( istack_l_filter_0(0:nodtot,3) )
       allocate( inod_4_l_filter_0(nodtot,3) )
       allocate( item_l_filter_0(ndep_1*nodtot,3) )
       allocate( inod_f_dist_l_0(ndep_1*nodtot,3) )
       allocate( coef_l_filter_0(ndep_1*nodtot,3) )
!
       inod_f_dist_l = 0
!
       inod_4_l_filter_0 = 0
       istack_l_filter_0 = 0
       item_l_filter_0 = 0
       inod_f_dist_l_0 = 0
       coef_l_filter_0 = 0.0d0
!
       end subroutine allocate_neighboring_nod_line
!
!  ----------------------------------------------------------------------!
       subroutine deallocate_neighboring_nod_line
!
       deallocate( inod_f_dist_l )
!
       deallocate( istack_l_filter_0 )
       deallocate( inod_4_l_filter_0 )
       deallocate( item_l_filter_0 )
       deallocate( inod_f_dist_l_0 )
       deallocate( coef_l_filter_0 )
!
       end subroutine deallocate_neighboring_nod_line
!
!  ----------------------------------------------------------------------!
!
      subroutine write_neighboring_nod_line
!
      use filter_moment_data_IO
      use filter_moment_data_IO_b
      use write_line_filter_data
!
      integer(kind = kint) :: i
!
!
       i_st2 =  max(i_st-ndepth,1)
       i_end2 = min(i_end+ndepth,nx)
       j_st2 =  max(j_st-ndepth,1)
       j_end2 = min(j_end+ndepth,ny)
       k_st2 =  max(k_st-ndepth,1)
       k_end2 = min(k_end+ndepth,nz)
!
       call allocate_neighboring_nod_line
!
       call set_fiilter_nod_line
!
       call allocate_l_filtering_data(nodtot)
       call order_fiilter_nod_line
!
       call write_filter_elen_data(filter_file_code)
!
       ndepth_l = ndepth
       num_filter_l = ndep_1
!
       call write_line_filter_data_a(nb_out, nodtot)
!
!   for debugging
!
!       write(nb_out,*) '!  xi direction'
!       call write_filter_nod_line(ione)
!       write(nb_out,*) '!  eta direction'
!       call write_filter_nod_line(itwo)
!       write(nb_out,*) '!   zi direction'
!       call write_filter_nod_line(ithree)
!
          write(nb_out,*) '! distance in x-direction'
          write(nb_out,'(10i16)')                                       &
     &               (inod_f_dist_l(i,1),i = 1, num_l_filter(1))
          write(nb_out,*) '! distance in y-direction'
          write(nb_out,'(10i16)')                                       &
     &               (inod_f_dist_l(i,2),i = 1, num_l_filter(2))
          write(nb_out,*) '! distance in z-direction'
          write(nb_out,'(10i16)')                                       &
     &               (inod_f_dist_l(i,3),i = 1, num_l_filter(3))
!
!
       call deallocate_neighboring_nod_line
       call deallocate_l_filtering_data
!
       end subroutine write_neighboring_nod_line
!
!  ----------------------------------------------------------------------!
!
      subroutine write_neighboring_nod_line_b
!
      use filter_moment_data_IO
      use filter_moment_data_IO_b
      use write_line_filter_data
!
!
       i_st2 =  max(i_st-ndepth,1)
       i_end2 = min(i_end+ndepth,nx)
       j_st2 =  max(j_st-ndepth,1)
       j_end2 = min(j_end+ndepth,ny)
       k_st2 =  max(k_st-ndepth,1)
       k_end2 = min(k_end+ndepth,nz)
!
       call allocate_neighboring_nod_line
!
       call set_fiilter_nod_line
!
       call allocate_l_filtering_data(nodtot)
       call order_fiilter_nod_line
!
!
       call write_filter_elen_data_b(filter_file_code)
!
       ndepth_l = ndepth
       num_filter_l = ndep_1
!
       call write_line_filter_data_b(nb_out, nodtot)

!
!       write(nb_out) '!   distance '
          write(nb_out) inod_f_dist_l
          write(nb_out) inod_f_dist_l
          write(nb_out) inod_f_dist_l
!
       call deallocate_neighboring_nod_line
       call deallocate_l_filtering_data
!
       end subroutine write_neighboring_nod_line_b
!
!  ----------------------------------------------------------------------!
       subroutine set_fiilter_nod_line
!
       integer(kind = kint) :: i, j, k, inod, nd
       integer(kind = kint) :: ii, i1, jj, kk, ifil, idx1, idx2, idx3
       integer(kind = kint), dimension(3) :: iflag
!
!
       idx1 = 0
       idx2 = 0
       idx3 = 0
       do nd = 1, 3
         istack_l_filter_0(0,nd) = 0
       end do
       do inod = 1, nodtot
         do nd = 1, 3
           inod_4_l_filter_0(inod,nd) = inod
         end do
!
!         if ( inod .le. intnodtot) then
           i = inod_table(inod,1)
           j = inod_table(inod,2)
           k = inod_table(inod,3)
           do i1 = 1, ndep_1
!
             do nd = 1, 3
               iflag(nd) = 1
             end do
!
             do ifil = 1, nf_type
               if ( abs(filter_c_x(i1,i,j,k,ifil)) .lt. eps_filter )  &
     &              iflag(1) = iflag(1)*0
               if ( abs(filter_c_y(i1,i,j,k,ifil)) .lt. eps_filter )  &
     &              iflag(2) = iflag(2)*0
               if ( abs(filter_c_z(i1,i,j,k,ifil)) .lt. eps_filter )  &
     &              iflag(3) = iflag(3)*0
             end do
      
!
             if ( iflag(1).eq.1 ) then
               idx1 = idx1 + 1
               ii = inod_f_item_x(i1,i,j,k)
               item_l_filter_0(idx1,1) = node_id_lc(ii,j,k)
               inod_f_dist_l_0(idx1,1) = inod_f_dist_x(i1,i,j,k)
               coef_l_filter_0(idx1,1) = filter_c_x(i1,i,j,k,1)
             end if
             if ( iflag(2).eq.1 ) then
               idx2 = idx2 + 1
               jj = inod_f_item_y(i1,i,j,k)
               item_l_filter_0(idx2,2) = node_id_lc(i,jj,k)
               inod_f_dist_l_0(idx2,2) = inod_f_dist_y(i1,i,j,k)
               coef_l_filter_0(idx2,2) = filter_c_y(i1,i,j,k,1)
             end if
             if ( iflag(3).eq.1 ) then
               idx3 = idx3 + 1
               kk = inod_f_item_z(i1,i,j,k)
               item_l_filter_0(idx3,3) = node_id_lc(i,j,kk)
               inod_f_dist_l_0(idx3,3) = inod_f_dist_z(i1,i,j,k)
               coef_l_filter_0(idx3,3) = filter_c_z(i1,i,j,k,1)
             end if
!
           end do
!         end if
!
         istack_l_filter_0(inod,1) = idx1
         istack_l_filter_0(inod,2) = idx2
         istack_l_filter_0(inod,3) = idx3
!
       end do
!
       do nd = 1, 3
         num_l_filter(nd) = istack_l_filter_0(nodtot,nd)
         nmax_l_filter(nd) = 0
         do inod = 1, nodtot
           ii = istack_l_filter_0(inod,nd) - istack_l_filter_0(inod-1,nd)
           nmax_l_filter(nd) = max(nmax_l_filter(nd),ii)
         end do
         nmin_l_filter(nd) = nmax_l_filter(nd)
         do inod = 1, nodtot
           ii = istack_l_filter_0(inod,nd) - istack_l_filter_0(inod-1,nd)
           nmin_l_filter(nd) = min(nmin_l_filter(nd),ii)
         end do
       end do
!
       end subroutine set_fiilter_nod_line
!
!  ----------------------------------------------------------------------!
       subroutine order_fiilter_nod_line
!
       integer(kind = kint) :: j, inod, jnod, nd
       integer(kind = kint) :: jj, kk, idx, jdx
!
!
       do nd = 1, 3
        jdx = 0
        jnod = 0
        istack_l_filter(0,nd) = 0
        do kk = nmax_l_filter(nd), nmin_l_filter(nd), -1
!
         do inod = 1, nodtot
           jj = istack_l_filter_0(inod,nd) - istack_l_filter_0(inod-1,nd)
           if ( jj .eq. kk) then
             jnod = jnod + 1
             inod_l_filter(jnod,nd) = inod
!
             do j = 1, jj
               jdx = jdx + 1
               idx = istack_l_filter_0(inod-1,nd) + j
               item_l_filter(jdx,nd) = item_l_filter_0(idx,nd)
               inod_f_dist_l(jdx,nd) = inod_f_dist_l_0(idx,nd)
               coef_l_filter(jdx,nd) = coef_l_filter_0(idx,nd)
             end do
!
             istack_l_filter(jnod,nd) = jdx
           end if
         end do
        end do
       end do
!
       end subroutine order_fiilter_nod_line
!
!  ----------------------------------------------------------------------
!
      subroutine write_filter_nod_line(nd)
!
       integer(kind = kint) :: nd
       integer(kind = kint) :: inod
       integer(kind = kint) :: ist, ied, num
!
      character(len=kchara) :: fmt_txt
!
!
      do inod = 1, nodtot
        ist = istack_l_filter(inod-1,nd) + 1
        ied = istack_l_filter(inod,nd)
        num = ied - ist + 1
!
        write(fmt_txt,'(a1,i3,a6)')  '(', num, '(i15))'
        write(22,fmt_txt) item_l_filter(ist:ied,nd)
        write(22,fmt_txt) inod_f_dist_l(ist:ied,nd)
!
        write(fmt_txt,'(a1,i3,a13)')  '(', num, '(1pE25.15e3))'
        write(22,fmt_txt) coef_l_filter(ist:ied,nd)
       end do
!
      end subroutine write_filter_nod_line
!
!  ----------------------------------------------------------------------!
      end module m_neib_nod_line_cube

