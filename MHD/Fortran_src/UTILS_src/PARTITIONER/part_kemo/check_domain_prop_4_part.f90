!check_domain_prop_4_part.f90
!      module check_domain_prop_4_part
!
!      Written by H. Matsui on Aug., 2007
!
!!      subroutine open_partition_log(num_domain, numedge,              &
!!     &          org_mesh_header, itl_nod_part, itl_ele_part,          &
!!     &          nod_d_grp, ele_d_grp)
!!        type(internal_4_partitioner), intent(in) :: itl_nod_part
!!        type(internal_4_partitioner), intent(in) :: itl_ele_part
!!        type(domain_group_4_partition), intent(in)  :: nod_d_grp
!!        type(domain_group_4_partition), intent(in)  :: ele_d_grp
!!      subroutine write_neighboring_pes(ip)
!!
!!      subroutine cal_edgecut(nedge, nnod_4_edge, ie_edge, nod_d_grp)
!!        type(domain_group_4_partition), intent(in)  :: nod_d_grp
!!      subroutine count_overlapped_ele(nele, nnod_4_ele, ie, nod_d_grp)
!!        type(domain_group_4_partition), intent(in)  :: nod_d_grp
!!      subroutine check_surface_def_in_surf_grp(numele, num_surf_bc,   &
!!     &          elmtyp, surf_item)
!
      module check_domain_prop_4_part
!
      use m_precision
      use t_domain_group_4_partition
      use t_internal_4_partitioner
!
      implicit none
!
      integer(kind = kint) :: NUM_EDGECUT = 0
      integer(kind = kint) :: NUM_OVERLAP_ELE
      private :: NUM_EDGECUT, NUM_OVERLAP_ELE
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine open_partition_log(num_domain, numedge,                &
     &          org_mesh_header, itl_nod_part, itl_ele_part,            &
     &          nod_d_grp, ele_d_grp)
!
      integer(kind = kint), intent(in) :: num_domain, numedge
      character(len = kchara), intent(in) :: org_mesh_header
      type(internal_4_partitioner), intent(in) :: itl_nod_part
      type(internal_4_partitioner), intent(in) :: itl_ele_part
      type(domain_group_4_partition), intent(in)  :: nod_d_grp
      type(domain_group_4_partition), intent(in)  :: ele_d_grp
!
      integer(kind = kint) :: ip, icou
!
!
!      open (21,file='partition.log',status='unknown')
!
      write (*,'(/,"*** GRID  file   ", a)')  trim(org_mesh_header)
      write (*,'(/,i5, " PEs")') num_domain
!
      if(NUM_EDGECUT .gt. 0) then
        write (*,'(/,"Total edge     #   ", i12)') numedge
        write (*,'(  "Total edge cut #   ", i12)') NUM_EDGECUT
      end if

      write (*,'(/,"Overlapped elements", i12)')  NUM_OVERLAP_ELE
!
!
      write (*,'(/,"MAX. internal node/PE ", i12)')                     &
     &     itl_nod_part%nmax_inter_sub
      write (*,'(  "MIN. internal node/PE ", i12)')                     &
     &     itl_nod_part%nmin_inter_sub
      write (*,'(  "MAX.cell/PE        ", i12)') itl_ele_part%nmax_sub
      write (*,'(  "MIN.cell/PE        ", i12)') itl_ele_part%nmin_sub
!
      write (*,'(/,"TOTAL NODE     #   ", i12)') nod_d_grp%num_s_domin
      write (*,'(  "TOTAL CELL     #   ", i12)') ele_d_grp%num_s_domin
      write (*,'(/," PE    NODE#   CELL#   EXT_CELL#")')
!
      do ip= 1, num_domain
        icou= itl_ele_part%num_4_subdomain(ip)                          &
     &       - itl_ele_part%num_inter_sub(ip)
        write (*,'(i3,5i16)') ip, itl_nod_part%num_inter_sub(ip),       &
     &                        itl_ele_part%num_4_subdomain(ip), icou
      enddo
!
      write (*,'(/," PE/NEIB-PE#    NEIB-PEs")')
!
      end subroutine open_partition_log
!
!   --------------------------------------------------------------------
!
      subroutine write_neighboring_pes(ip, new_comm)
!
      use t_comm_table
!
      integer(kind = kint), intent(in) :: ip
      type(communication_table), intent(in) :: new_comm
!
!
      write (*,'(i6,i15,255i6)') ip, new_comm%num_neib,                 &
     &     new_comm%id_neib(1:new_comm%num_neib)
!
      end subroutine write_neighboring_pes
!
!   --------------------------------------------------------------------
!
      subroutine cal_edgecut(nedge, nnod_4_edge, ie_edge, nod_d_grp)
!
      integer(kind = kint), intent(in) :: nedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(nedge,nnod_4_edge)
      type(domain_group_4_partition), intent(in)  :: nod_d_grp
!
      integer(kind = kint) :: k1, ie, in1, in2
!C
!C-- calc. EDGECUT
      NUM_EDGECUT = 0
      do k1 = 1, nnod_4_edge-1
        do ie= 1, nedge
          in1= ie_edge(ie,k1  )
          in2= ie_edge(ie,k1+1)
          if(nod_d_grp%IGROUP(in1) .ne. nod_d_grp%IGROUP(in2)) then
            NUM_EDGECUT = NUM_EDGECUT + 1
          end if
        end do
      end do
!
      end subroutine cal_edgecut
!
!   --------------------------------------------------------------------
!
      subroutine count_overlapped_ele(nele, nnod_4_ele, ie, nod_d_grp)
!
      integer(kind = kint), intent(in) :: nele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(nele,nnod_4_ele)
      type(domain_group_4_partition), intent(in)  :: nod_d_grp
!
      integer(kind = kint) :: iele, k1, k2, inod1, inod2, iflag
!
      NUM_OVERLAP_ELE = 0
      do iele= 1, nele
        iflag = 0
        do k1= 1, nnod_4_ele
          inod1 = ie(iele,k1)
          do k2= 1, nnod_4_ele
            inod2 = ie(iele,k2)
            if(nod_d_grp%IGROUP(inod1) .ne. nod_d_grp%IGROUP(inod2))    &
     &       then
              iflag = 1
              go to 10
            end if
          end do
        end do
 10     continue
        if (iflag .eq. 1) NUM_OVERLAP_ELE = NUM_OVERLAP_ELE + 1
      end do
!
      end subroutine count_overlapped_ele
!
!   --------------------------------------------------------------------
!
      subroutine check_surface_def_in_surf_grp(numele, num_surf_bc,     &
     &          elmtyp, surf_item)
!
      integer(kind = kint), intent(in) :: numele, num_surf_bc
      integer(kind = kint), intent(in) :: elmtyp(numele)
      integer(kind = kint), intent(in) :: surf_item(2,num_surf_bc)
!
      integer(kind = kint) :: nmax_sf_ele
      integer(kind = kint) :: in, is
!
!
      do is = 1, num_surf_bc
        in  = surf_item(1,is)
!
        if     (elmtyp(in).eq.111 .or. elmtyp(in).eq.112                &
     &     .or. elmtyp(in).eq.611 .or. elmtyp(in).eq.612) then
          nmax_sf_ele = 2
        else if(elmtyp(in).eq.211 .or. elmtyp(in).eq.212                &
     &     .or. elmtyp(in).eq.711 .or. elmtyp(in).eq.712) then
          nmax_sf_ele = 3
        else if(elmtyp(in).eq.221 .or. elmtyp(in).eq.222                &
     &     .or. elmtyp(in).eq.223 .or. elmtyp(in).eq.311                &
     &     .or. elmtyp(in).eq.312 .or. elmtyp(in).eq.721                &
     &     .or. elmtyp(in).eq.722) then
          nmax_sf_ele = 4
        else if(elmtyp(in).eq.321 .or. elmtyp(in).eq.322) then
          nmax_sf_ele = 5
        else if(elmtyp(in).eq.331 .or. elmtyp(in).eq.332                &
     &     .or. elmtyp(in).eq.333) then
          nmax_sf_ele = 6
        else
          nmax_sf_ele = 6
        end if
!
        if ( surf_item(2,is) .gt. nmax_sf_ele) then
          write (*,'(/,a,i12/)')                                        &
     &        " ### ABORT : local surface ID inconsistent in SUF.GRP.", &
     &          is
          stop
        end if
!
      end do
!
      end subroutine check_surface_def_in_surf_grp
!
!   --------------------------------------------------------------------
!
      end module check_domain_prop_4_part
