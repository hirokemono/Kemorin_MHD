!
!      module neib_nod_line_cube
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
!!      subroutine allocate_neighboring_nod_line
!!      subroutine deallocate_neighboring_nod_line
!!      subroutine write_neighboring_nod_line                           &
!!     &         (id_rank, nf_type, c_size, c_each, FEM_elen)
!!        type(size_of_cube), intent(in) :: c_size
!!        type(size_of_each_cube), intent(in) :: c_each
!!        type(gradient_model_data_type), intent(inout) :: FEM_elen
!
      module neib_nod_line_cube
!
      use m_precision
      use m_constants
!
      use t_size_of_cube
      use m_local_node_id_cube
      use m_cube_files_data
      use m_filtering_nod_4_cubmesh
      use m_filter_data_4_plane
!
      use t_l_filtering_data
!
      implicit none
!
!
      type(line_filtering_type), save :: fil_l1
!
      integer(kind = kint), allocatable :: inod_f_dist_l(:,:)
!
      integer(kind = kint), allocatable :: inod_4_l_filter_0(:,:)
      integer(kind = kint), allocatable :: istack_l_filter_0(:,:)
!
      integer(kind = kint), allocatable :: item_l_filter_0(:,:)
      integer(kind = kint), allocatable :: inod_f_dist_l_0(:,:)
      real(kind = kreal), allocatable :: coef_l_filter_0(:,:)
!
      private ::  fil_l1
      private ::  inod_f_dist_l
      private ::  inod_4_l_filter_0, istack_l_filter_0
      private ::  item_l_filter_0, inod_f_dist_l_0, coef_l_filter_0
!
      private :: allocate_neighboring_nod_line
      private :: deallocate_neighboring_nod_line
      private :: set_fiilter_nod_line, order_fiilter_nod_line
      private :: write_filter_nod_line
!
!  ----------------------------------------------------------------------
!
      contains
!
!  ----------------------------------------------------------------------!
      subroutine write_neighboring_nod_line                             &
     &         (id_rank, nf_type, c_size, c_each, FEM_elen)
!
      use t_filter_elength
      use filter_mom_type_data_IO
      use set_parallel_file_name
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: nf_type
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(gradient_model_data_type), intent(inout) :: FEM_elen
!
      integer(kind = kint) :: i
!
!
       call allocate_neighboring_nod_line(c_size, c_each)
!
       call set_fiilter_nod_line(nf_type, c_size, c_each, fil_l1)
!
       call alloc_l_filtering_data                                      &
     &    (c_each%nodtot, c_size%ndepth, c_size%ndep_1, fil_l1)
       call order_fiilter_nod_line(c_each, fil_l1)
!
!
       nb_name = add_process_id(id_rank, filter_file_header)
       write(*,*) 'output ascii file: ', trim(nb_name)
       open (nb_out, file=nb_name)
!
       call write_filter_elen_data_type(nb_out, FEM_elen)
!
       call write_line_filter_data_a(nb_out, fil_l1)
!
!   for debugging
!
!       write(nb_out,*) '!  xi direction'
!       call write_filter_nod_line(ione, c_each, fil_l1)
!       write(nb_out,*) '!  eta direction'
!       call write_filter_nod_line(itwo, c_each, fil_l1)
!       write(nb_out,*) '!   zi direction'
!       call write_filter_nod_line(ithree, c_each, fil_l1)
!
          write(nb_out,*) '! distance in x-direction'
          write(nb_out,'(10i16)')                                       &
     &               (inod_f_dist_l(i,1),i = 1, fil_l1%num_lf(1))
          write(nb_out,*) '! distance in y-direction'
          write(nb_out,'(10i16)')                                       &
     &               (inod_f_dist_l(i,2),i = 1, fil_l1%num_lf(2))
          write(nb_out,*) '! distance in z-direction'
          write(nb_out,'(10i16)')                                       &
     &               (inod_f_dist_l(i,3),i = 1, fil_l1%num_lf(3))
       close(nb_out)
!
!
       call deallocate_neighboring_nod_line
       call dealloc_l_filtering_data(fil_l1)
!
       end subroutine write_neighboring_nod_line
!
!  ----------------------------------------------------------------------
!  ----------------------------------------------------------------------!
       subroutine allocate_neighboring_nod_line(c_size, c_each)
!
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
!
       integer(kind = kint) :: num
!
!
       num = c_size%ndep_1 * c_each%nodtot
       allocate( inod_f_dist_l(num,3) )
!
       allocate( istack_l_filter_0(0:c_each%nodtot,3) )
       allocate( inod_4_l_filter_0(c_each%nodtot,3) )
       allocate( item_l_filter_0(num,3) )
       allocate( inod_f_dist_l_0(num,3) )
       allocate( coef_l_filter_0(num,3) )
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
!  ----------------------------------------------------------------------
!
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
!  ----------------------------------------------------------------------
!  ----------------------------------------------------------------------
!
      subroutine set_fiilter_nod_line(nf_type, c_size, c_each, fil_l)
!
      integer(kind = kint), intent(in) :: nf_type
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(in) :: c_each
      type(line_filtering_type), intent(inout) :: fil_l
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
       do inod = 1, c_each%nodtot
         do nd = 1, 3
           inod_4_l_filter_0(inod,nd) = inod
         end do
!
!         if ( inod .le. c_each%intnodtot) then
           i = inod_table(inod,1)
           j = inod_table(inod,2)
           k = inod_table(inod,3)
           do i1 = 1, c_size%ndep_1
!
             do nd = 1, 3
               iflag(nd) = 1
             end do
!
             do ifil = 1, nf_type
               if(abs(c_fil_nod%filter_c_x(i1,i,j,k,ifil))              &
     &            .lt. eps_filter) iflag(1) = iflag(1)*0
               if(abs(c_fil_nod%filter_c_y(i1,i,j,k,ifil))              &
     &            .lt. eps_filter) iflag(2) = iflag(2)*0
               if(abs(c_fil_nod%filter_c_z(i1,i,j,k,ifil))              &
     &            .lt. eps_filter) iflag(3) = iflag(3)*0
             end do
      
!
             if ( iflag(1).eq.1 ) then
               idx1 = idx1 + 1
               ii = inod_f_item_x(i1,i,j,k)
               item_l_filter_0(idx1,1) = node_id_lc(ii,j,k)
               inod_f_dist_l_0(idx1,1) = inod_f_dist_x(i1,i,j,k)
               coef_l_filter_0(idx1,1)                                  &
     &              = c_fil_nod%filter_c_x(i1,i,j,k,1)
             end if
             if ( iflag(2).eq.1 ) then
               idx2 = idx2 + 1
               jj = inod_f_item_y(i1,i,j,k)
               item_l_filter_0(idx2,2) = node_id_lc(i,jj,k)
               inod_f_dist_l_0(idx2,2) = inod_f_dist_y(i1,i,j,k)
               coef_l_filter_0(idx2,2)                                  &
     &              = c_fil_nod%filter_c_y(i1,i,j,k,1)
             end if
             if ( iflag(3).eq.1 ) then
               idx3 = idx3 + 1
               kk = inod_f_item_z(i1,i,j,k)
               item_l_filter_0(idx3,3) = node_id_lc(i,j,kk)
               inod_f_dist_l_0(idx3,3) = inod_f_dist_z(i1,i,j,k)
               coef_l_filter_0(idx3,3)                                  &
     &              = c_fil_nod%filter_c_z(i1,i,j,k,1)
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
         fil_l%num_lf(nd) = istack_l_filter_0(c_each%nodtot,nd)
         fil_l%nmax_lf(nd) = 0
         do inod = 1, c_each%nodtot
           ii = istack_l_filter_0(inod,nd) - istack_l_filter_0(inod-1,nd)
           fil_l%nmax_lf(nd) = max(fil_l%nmax_lf(nd),ii)
         end do
         fil_l%nmin_lf(nd) = fil_l%nmax_lf(nd)
         do inod = 1, c_each%nodtot
           ii = istack_l_filter_0(inod,nd) - istack_l_filter_0(inod-1,nd)
           fil_l%nmin_lf(nd) = min(fil_l%nmin_lf(nd),ii)
         end do
       end do
!
       end subroutine set_fiilter_nod_line
!
!  ----------------------------------------------------------------------
!
      subroutine order_fiilter_nod_line(c_each, fil_l)
!
      type(size_of_each_cube), intent(in) :: c_each
      type(line_filtering_type), intent(inout) :: fil_l
!
       integer(kind = kint) :: j, inod, jnod, nd
       integer(kind = kint) :: jj, kk, idx, jdx
!
!
       do nd = 1, 3
        jdx = 0
        jnod = 0
        fil_l%istack_lf(0,nd) = 0
        do kk = fil_l%nmax_lf(nd), fil_l%nmin_lf(nd), -1
!
         do inod = 1, c_each%nodtot
           jj = istack_l_filter_0(inod,nd) - istack_l_filter_0(inod-1,nd)
           if ( jj .eq. kk) then
             jnod = jnod + 1
             fil_l%inod_lf(jnod,nd) = inod
!
             do j = 1, jj
               jdx = jdx + 1
               idx = istack_l_filter_0(inod-1,nd) + j
               fil_l%item_lf(jdx,nd) = item_l_filter_0(idx,nd)
               inod_f_dist_l(jdx,nd) = inod_f_dist_l_0(idx,nd)
               fil_l%coef_l(jdx,nd) =  coef_l_filter_0(idx,nd)
             end do
!
             fil_l%istack_lf(jnod,nd) = jdx
           end if
         end do
        end do
       end do
!
       end subroutine order_fiilter_nod_line
!
!  ----------------------------------------------------------------------
!
      subroutine write_filter_nod_line(nd, c_each, fil_l)
!
      type(size_of_each_cube), intent(in) :: c_each
!
      integer(kind = kint), intent(in) :: nd
      type(line_filtering_type), intent(in) :: fil_l
!
       integer(kind = kint) :: inod
       integer(kind = kint) :: ist, ied, num
!
      character(len=kchara) :: fmt_txt
!
!
      do inod = 1, c_each%nodtot
        ist = fil_l%istack_lf(inod-1,nd) + 1
        ied = fil_l%istack_lf(inod,nd)
        num = ied - ist + 1
!
        write(fmt_txt,'(a1,i3,a6)')  '(', num, '(i15))'
        write(22,fmt_txt) fil_l%item_lf(ist:ied,nd)
        write(22,fmt_txt) inod_f_dist_l(ist:ied,nd)
!
        write(fmt_txt,'(a1,i3,a13)')  '(', num, '(1pE25.15e3))'
        write(22,fmt_txt) fil_l%coef_l(ist:ied,nd)
       end do
!
      end subroutine write_filter_nod_line
!
!  ----------------------------------------------------------------------
!
      end module neib_nod_line_cube

