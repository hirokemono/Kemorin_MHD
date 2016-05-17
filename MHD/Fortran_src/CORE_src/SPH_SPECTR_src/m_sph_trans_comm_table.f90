!>@file   m_sph_trans_comm_table.f90
!!@brief  module m_sph_trans_comm_table
!!
!!@author H. Matsui
!!@date Programmed in July, 2007 
!
!>@brief  Communication tables for spherical transform
!!
      module m_sph_trans_comm_table
!
      use m_precision
      use t_sph_trans_comm_tbl
!
      implicit none
!
!>  Structure for communication table for spherical transform
      type(sph_comm_tables), save :: comms_sph1
!comms_sph1%comm_rlm
!
!>        Communication table for @f$ f(r,t,p) @f$ 
!      type(sph_comm_tbl), save :: comm_rtp1
!>        Communication table for @f$ f(r,t,m) @f$ 
!      type(sph_comm_tbl), save :: comm_rtm1
!>        Communication table for @f$ f(r,l,m) @f$ 
!      type(sph_comm_tbl), save :: comm_rlm1
!>        Communication table for @f$ f(r,j) @f$ 
!      type(sph_comm_tbl), save :: comm_rj1
!
      end module m_sph_trans_comm_table
